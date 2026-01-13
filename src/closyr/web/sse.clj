(ns closyr.web.sse
  "Server-Sent Events (SSE) implementation for streaming solver progress."
  (:require
    [cheshire.core :as json]
    [clojure.core.async :as async :refer [<! >! chan close! go-loop timeout]]
    [closyr.util.log :as log]
    [ring.core.protocols :as ring-protocols])
  (:import
    (java.io OutputStream)))


(set! *warn-on-reflection* true)


(defn format-sse-event
  "Format a map as an SSE event string.
   Supports optional event type."
  ([data]
   (format-sse-event nil data))
  ([event-type data]
   (let [json-data (json/encode data)]
     (str (when event-type
            (str "event: " event-type "\n"))
          "data: " json-data "\n\n"))))


(defn create-event-channel
  "Create a channel for SSE events.
   Returns {:channel ch :send! fn :close! fn}"
  []
  (let [ch (chan 100)]
    {:channel ch
     :send!   (fn [event-type data]
                (async/put! ch {:event event-type :data data}))
     :close!  (fn []
                (close! ch))}))


;; Custom type for SSE streaming that implements Ring's protocol
(deftype SSEBody [event-chan]
  ring-protocols/StreamableResponseBody
  (write-body-to-stream [_ response output-stream]
    (let [^OutputStream os output-stream]
      (try
        (loop []
          (when-let [{:keys [event data]} (async/<!! event-chan)]
            (let [^String sse-str (format-sse-event event data)
                  bytes (.getBytes sse-str "UTF-8")]
              (.write os bytes)
              (.flush os)
              (recur))))
        (catch Exception e
          (log/debug "SSE stream closed:" (.getMessage e)))
        (finally
          (close! event-chan)
          (try (.close os)
               (catch Exception _)))))))


(defn event-stream-response
  "Create a Ring response that streams SSE events from a channel.
   The channel should receive maps with :event and :data keys.

   Usage:
   (let [{:keys [channel send! close!]} (create-event-channel)]
     ;; In another thread/future:
     (send! \"progress\" {:iteration 1 :total 10})
     (send! \"complete\" {:result ...})
     (close!)

     ;; Return response:
     (event-stream-response channel))"
  [event-chan]
  {:status  200
   :headers {"Content-Type"                "text/event-stream"
             "Cache-Control"               "no-cache"
             "Connection"                  "keep-alive"
             "Access-Control-Allow-Origin" "*"
             "X-Accel-Buffering"           "no"}
   :body    (->SSEBody event-chan)})


(defn send-heartbeat!
  "Send a heartbeat comment to keep the SSE connection alive.
   Call this periodically (e.g., every 15 seconds) for long-running operations."
  [^OutputStream output-stream]
  (try
    (.write output-stream (.getBytes ": heartbeat\n\n" "UTF-8"))
    (.flush output-stream)
    true
    (catch Exception _
      false)))
