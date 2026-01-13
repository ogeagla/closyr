(ns closyr.web.server
  "HTTP server lifecycle management using Jetty."
  (:require
    [closyr.util.log :as log]
    [closyr.web.routes :as routes]
    [ring.adapter.jetty :as jetty])
  (:import (org.eclipse.jetty.server Server)))


(set! *warn-on-reflection* true)


(defonce ^:private server* (atom nil))


(defn start!
  "Start the HTTP server on the given port.
   Returns the server instance."
  ([]
   (start! {}))
  ([{:keys [port] :or {port 3000}}]
   (when @server*
     (log/warn "Server already running, stopping first...")
     (.stop ^Server @server*))
   (log/info "Starting web server on port" port)
   (let [server (jetty/run-jetty #'routes/app
                                 {:port  port
                                  :join? false})]
     (reset! server* server)
     (log/info "Web server started at http://localhost:" port)
     server)))


(defn stop!
  "Stop the running HTTP server."
  []
  (when-let [^Server server @server*]
    (log/info "Stopping web server...")
    (.stop server)
    (reset! server* nil)
    (log/info "Web server stopped")))


(defn running?
  "Check if the server is currently running."
  []
  (some? @server*))


(comment
  (start!)
  (stop!))
