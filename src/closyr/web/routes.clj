(ns closyr.web.routes
  "Route definitions for the web application."
  (:require
    [closyr.web.handlers.api :as api]
    [closyr.web.handlers.pages :as pages]
    [closyr.web.middleware :as mw]
    [reitit.ring :as ring]
    [ring.middleware.defaults :refer [site-defaults wrap-defaults]]
    [ring.middleware.multipart-params :refer [wrap-multipart-params]]))


(set! *warn-on-reflection* true)


(def routes
  "Application routes."
  [["/" {:get pages/index}]
   ["/solver" {:get pages/solver}]

   ;; API endpoints
   ["/api"
    ["/solve" {:post api/solve}]
    ["/jobs/:id" {:get api/get-job}]
    ["/jobs/:id/stop" {:post api/stop-job}]
    ["/jobs/:id/pause" {:post api/pause-job}]
    ["/jobs/:id/resume" {:post api/resume-job}]
    ["/jobs/:id/events" {:get api/events}]
    ["/datasets" {:get api/datasets}]
    ["/upload-csv" {:post api/upload-csv}]]

   ;; HTMX partials
   ["/partials"
    ["/results/:id" {:get pages/results-partial}]]])


(def app
  "Ring handler with middleware."
  (ring/ring-handler
    (ring/router routes)

    ;; Default handlers for static files and 404
    (ring/routes
      (ring/create-resource-handler {:path "/"})
      (ring/create-default-handler))

    ;; Middleware stack
    {:middleware [;; Parse multipart form data (for file uploads)
                  wrap-multipart-params
                  ;; JSON body parsing
                  mw/wrap-json-body
                  ;; JSON response encoding
                  mw/wrap-json-response
                  ;; CORS headers
                  mw/wrap-cors
                  ;; Exception handling
                  mw/wrap-exceptions
                  ;; Standard site defaults (but disable CSRF for API)
                  [wrap-defaults (-> site-defaults
                                     (assoc-in [:security :anti-forgery] false)
                                     (assoc-in [:responses :content-types] false))]]}))
