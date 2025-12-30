(ns closyr.web.handlers.pages
  "HTML page handlers using Selmer templates."
  (:require
    [cheshire.core :as json]
    [closyr.web.handlers.api :as api]
    [selmer.parser :as selmer]))


(set! *warn-on-reflection* true)


;; Configure Selmer to use resources/templates
(selmer/set-resource-path! (clojure.java.io/resource "templates"))


(defn- render
  "Render a template with the given context."
  [template-name context]
  {:status  200
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body    (selmer/render-file template-name context)})


;; ============================================================================
;; Page Handlers
;; ============================================================================

(defn index
  "GET / - Landing page."
  [_]
  (render "index.html" {:title "Closyr - Symbolic Regression"}))


(defn solver
  "GET /solver - Main solver page with form."
  [_]
  ;; Get datasets for the preset dropdown
  (let [datasets-response (api/datasets nil)
        datasets (-> datasets-response :body (json/parse-string true) :datasets)]
    (render "solver.html" {:title    "Solver - Closyr"
                           :datasets datasets})))


;; ============================================================================
;; HTMX Partials
;; ============================================================================

(defn results-partial
  "GET /partials/results/:id - Render results partial for a job."
  [{:keys [path-params]}]
  (let [job-id (:id path-params)
        job (get @api/jobs* job-id)]
    (if job
      (render "partials/results.html" {:job-id job-id
                                       :job    job})
      {:status  404
       :headers {"Content-Type" "text/html"}
       :body    "<div class=\"text-red-500\">Job not found</div>"})))


(defn job-started-partial
  "Render the SSE-connected progress partial after job submission."
  [job-id]
  {:status  200
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body    (selmer/render-file "partials/progress.html" {:job-id job-id})})
