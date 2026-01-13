(ns closyr.web.middleware
  "Ring middleware for JSON parsing, CORS, and error handling."
  (:require
    [cheshire.core :as json]
    [closyr.util.log :as log]))


(set! *warn-on-reflection* true)


(defn wrap-json-body
  "Middleware to parse JSON request bodies into :body-params."
  [handler]
  (fn [request]
    (let [content-type (get-in request [:headers "content-type"] "")
          body (:body request)]
      (if (and body
               (instance? java.io.InputStream body)
               (or (.contains ^String content-type "application/json")
                   (.contains ^String content-type "text/json")))
        (try
          (let [body-str (slurp body)
                body-params (when (seq body-str)
                              (json/parse-string body-str true))]
            (handler (assoc request :body-params body-params)))
          (catch Exception e
            (log/warn "Failed to parse JSON body:" (.getMessage e))
            {:status 400
             :headers {"Content-Type" "application/json"}
             :body (json/encode {:error "Invalid JSON body"})}))
        (handler request)))))


(defn wrap-json-response
  "Middleware to encode response bodies as JSON when appropriate."
  [handler]
  (fn [request]
    (let [response (handler request)]
      (if (and (map? (:body response))
               (not (get-in response [:headers "Content-Type"])))
        (-> response
            (assoc-in [:headers "Content-Type"] "application/json")
            (update :body json/encode))
        response))))


(defn wrap-cors
  "Middleware to add CORS headers for API access."
  [handler]
  (fn [request]
    (let [response (handler request)]
      (-> response
          (assoc-in [:headers "Access-Control-Allow-Origin"] "*")
          (assoc-in [:headers "Access-Control-Allow-Methods"] "GET, POST, OPTIONS")
          (assoc-in [:headers "Access-Control-Allow-Headers"] "Content-Type")))))


(defn wrap-exceptions
  "Middleware to catch exceptions and return JSON error responses."
  [handler]
  (fn [request]
    (try
      (handler request)
      (catch Exception e
        (log/error "Unhandled exception in request handler:" (.getMessage e))
        {:status 500
         :headers {"Content-Type" "application/json"}
         :body (json/encode {:error "Internal server error"
                             :message (.getMessage e)})}))))
