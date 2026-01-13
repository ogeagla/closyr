(ns closyr.web-test
  "Tests for the HTTP server and API endpoints."
  (:require
    [cheshire.core :as json]
    [clojure.test :refer :all]
    [closyr.test-utils :as test-utils]
    [closyr.web.handlers.api :as api]
    [closyr.web.middleware :as mw]
    [closyr.web.routes :as routes]
    [closyr.web.server :as server])
  (:import
    (java.io ByteArrayInputStream)))


(use-fixtures :once test-utils/quiet-logging-fixture)


;; ============================================================================
;; Helper functions
;; ============================================================================

(defn- json-body
  "Create a JSON body as an InputStream from a map."
  [data]
  (ByteArrayInputStream. (.getBytes (json/encode data) "UTF-8")))


(defn- parse-json-body
  "Parse JSON response body."
  [response]
  (when (:body response)
    (json/parse-string (:body response) true)))


(defn- make-request
  "Helper to create a ring request map."
  [method uri & {:keys [body headers params]}]
  (cond-> {:request-method method
           :uri uri}
    body (assoc :body (json-body body))
    headers (assoc :headers (merge {"content-type" "application/json"} headers))
    body (assoc :headers (merge {"content-type" "application/json"} headers))
    params (assoc :path-params params)))


;; ============================================================================
;; Server lifecycle tests
;; ============================================================================

(deftest test-server-not-running-initially
  (testing "Server is not running when not started"
    ;; Make sure server is stopped first
    (server/stop!)
    (is (not (server/running?)))))


(deftest test-server-start-stop
  (testing "Server can start and stop"
    (try
      (let [srv (server/start! {:port 3333})]
        (is (some? srv))
        (is (server/running?))
        (server/stop!)
        (is (not (server/running?))))
      (finally
        (server/stop!)))))


(deftest test-server-restart
  (testing "Starting server while running restarts it"
    (try
      (server/start! {:port 3334})
      (is (server/running?))
      ;; Start again on different port
      (server/start! {:port 3335})
      (is (server/running?))
      (finally
        (server/stop!)))))


;; ============================================================================
;; Routes and app handler tests
;; ============================================================================

(deftest test-routes-defined
  (testing "Routes are defined"
    (is (vector? routes/routes))
    (is (pos? (count routes/routes)))))


(deftest test-app-handler-exists
  (testing "App handler is a function"
    (is (fn? routes/app))))


(deftest test-404-for-unknown-route
  (testing "Unknown routes return 404"
    (let [response (routes/app {:request-method :get
                                :uri "/unknown-route-xyz"})]
      (is (= 404 (:status response))))))


;; ============================================================================
;; API: datasets endpoint tests
;; ============================================================================

(deftest test-datasets-endpoint
  (testing "GET /api/datasets returns dataset list"
    (let [response (api/datasets {})]
      (is (= 200 (:status response)))
      (is (= "application/json" (get-in response [:headers "Content-Type"])))
      (let [body (parse-json-body response)]
        (is (contains? body :datasets))
        (is (vector? (:datasets body)))
        (is (pos? (count (:datasets body))))))))


(deftest test-datasets-have-required-fields
  (testing "Datasets have required fields"
    (let [response (api/datasets {})
          body (parse-json-body response)
          datasets (:datasets body)]
      (doseq [ds datasets]
        (is (contains? ds :id) (str "Dataset missing :id: " ds))
        (is (contains? ds :name) (str "Dataset missing :name: " ds))
        (is (contains? ds :xs) (str "Dataset missing :xs: " ds))
        (is (contains? ds :ys) (str "Dataset missing :ys: " ds))))))


(deftest test-datasets-include-known-datasets
  (testing "Known datasets are included"
    (let [response (api/datasets {})
          body (parse-json-body response)
          ids (set (map :id (:datasets body)))]
      (is (contains? ids "h-line"))
      (is (contains? ids "nguyen4"))
      (is (contains? ids "nguyen5"))
      (is (contains? ids "feynman-lorentz"))
      (is (contains? ids "feynman-wave"))
      (is (contains? ids "primes-100")))))


;; ============================================================================
;; API: CSV upload endpoint tests
;; ============================================================================

(deftest test-upload-csv-basic
  (testing "CSV upload parses simple data"
    (let [response (api/upload-csv {:body-params {:content "1,2\n3,4\n5,6"}})]
      (is (= 200 (:status response)))
      (let [body (parse-json-body response)]
        (is (= [1.0 3.0 5.0] (:xs body)))
        (is (= [2.0 4.0 6.0] (:ys body)))
        (is (= 3 (:rowCount body)))))))


(deftest test-upload-csv-with-headers
  (testing "CSV upload handles headers"
    (let [response (api/upload-csv {:body-params {:content "x,y\n1,1\n2,4\n3,9"}})]
      (is (= 200 (:status response)))
      (let [body (parse-json-body response)]
        (is (= [1.0 2.0 3.0] (:xs body)))
        (is (= [1.0 4.0 9.0] (:ys body)))
        (is (= 3 (:rowCount body)))))))


(deftest test-upload-csv-missing-content
  (testing "CSV upload fails without content"
    (let [response (api/upload-csv {:body-params {}})]
      (is (= 400 (:status response)))
      (let [body (parse-json-body response)]
        (is (contains? body :error))))))


(deftest test-upload-csv-invalid-data
  (testing "CSV upload fails with invalid data"
    (let [response (api/upload-csv {:body-params {:content "not,valid\ncsv,data"}})]
      (is (= 400 (:status response)))
      (let [body (parse-json-body response)]
        (is (contains? body :error))))))


;; ============================================================================
;; API: solve endpoint tests
;; ============================================================================

(deftest test-solve-missing-params
  (testing "Solve fails without xs/ys"
    (let [response (api/solve {:body-params {}})]
      (is (= 400 (:status response)))
      (let [body (parse-json-body response)]
        (is (contains? body :error))))))


(deftest test-solve-missing-xs
  (testing "Solve fails without xs"
    (let [response (api/solve {:body-params {:ys [1 2 3]}})]
      (is (= 400 (:status response)))
      (let [body (parse-json-body response)]
        (is (contains? body :error))))))


(deftest test-solve-missing-ys
  (testing "Solve fails without ys"
    (let [response (api/solve {:body-params {:xs [1 2 3]}})]
      (is (= 400 (:status response)))
      (let [body (parse-json-body response)]
        (is (contains? body :error))))))


(deftest test-solve-returns-job-id
  (testing "Solve returns job ID and events URL"
    (let [response (api/solve {:body-params {:xs [1 2 3 4 5]
                                             :ys [1 4 9 16 25]
                                             :config {:iterations 1
                                                      :population 5}}})]
      (is (= 202 (:status response)))
      (let [body (parse-json-body response)]
        (is (contains? body :jobId))
        (is (contains? body :eventsUrl))
        (is (string? (:jobId body)))
        (is (.contains ^String (:eventsUrl body) (:jobId body)))))))


;; ============================================================================
;; API: job status endpoint tests
;; ============================================================================

(deftest test-get-job-not-found
  (testing "GET job returns 404 for unknown job"
    (let [response (api/get-job {:path-params {:id "nonexistent-job-id"}})]
      (is (= 404 (:status response)))
      (let [body (parse-json-body response)]
        (is (contains? body :error))))))


(deftest test-get-job-found
  (testing "GET job returns job status"
    ;; First create a job
    (let [solve-response (api/solve {:body-params {:xs [1 2 3]
                                                   :ys [2 4 6]
                                                   :config {:iterations 1
                                                            :population 5}}})
          solve-body (parse-json-body solve-response)
          job-id (:jobId solve-body)
          ;; Now get the job
          response (api/get-job {:path-params {:id job-id}})]
      (is (= 200 (:status response)))
      (let [body (parse-json-body response)]
        (is (contains? body :status))))))


;; ============================================================================
;; API: stop/pause/resume endpoint tests
;; ============================================================================

(deftest test-stop-job-not-found
  (testing "Stop returns 404 for unknown job"
    (let [response (api/stop-job {:path-params {:id "nonexistent-id"}})]
      (is (= 404 (:status response))))))


(deftest test-pause-job-not-found
  (testing "Pause returns 404 for unknown job"
    (let [response (api/pause-job {:path-params {:id "nonexistent-id"}})]
      (is (= 404 (:status response))))))


(deftest test-resume-job-not-found
  (testing "Resume returns 404 for unknown job"
    (let [response (api/resume-job {:path-params {:id "nonexistent-id"}})]
      (is (= 404 (:status response))))))


;; ============================================================================
;; Middleware tests
;; ============================================================================

(deftest test-wrap-json-body-parses-json
  (testing "JSON body middleware parses JSON"
    (let [handler (fn [req] {:status 200 :body (:body-params req)})
          wrapped (mw/wrap-json-body handler)
          request {:headers {"content-type" "application/json"}
                   :body (json-body {:foo "bar" :num 42})}
          response (wrapped request)]
      (is (= {:foo "bar" :num 42} (:body response))))))


(deftest test-wrap-json-body-handles-empty
  (testing "JSON body middleware handles empty body"
    (let [handler (fn [req] {:status 200 :body (:body-params req)})
          wrapped (mw/wrap-json-body handler)
          request {:headers {"content-type" "text/plain"}
                   :body nil}
          response (wrapped request)]
      (is (= 200 (:status response))))))


(deftest test-wrap-json-body-handles-invalid-json
  (testing "JSON body middleware returns 400 for invalid JSON"
    (let [handler (fn [req] {:status 200 :body "ok"})
          wrapped (mw/wrap-json-body handler)
          request {:headers {"content-type" "application/json"}
                   :body (ByteArrayInputStream. (.getBytes "not valid json"))}
          response (wrapped request)]
      (is (= 400 (:status response))))))


(deftest test-wrap-cors-adds-headers
  (testing "CORS middleware adds headers"
    (let [handler (fn [_] {:status 200 :headers {} :body "ok"})
          wrapped (mw/wrap-cors handler)
          response (wrapped {})]
      (is (= "*" (get-in response [:headers "Access-Control-Allow-Origin"])))
      (is (some? (get-in response [:headers "Access-Control-Allow-Methods"])))
      (is (some? (get-in response [:headers "Access-Control-Allow-Headers"]))))))


(deftest test-wrap-exceptions-catches-errors
  (testing "Exception middleware catches errors"
    (let [handler (fn [_] (throw (Exception. "Test error")))
          wrapped (mw/wrap-exceptions handler)
          response (wrapped {})]
      (is (= 500 (:status response)))
      (let [body (parse-json-body response)]
        (is (contains? body :error))))))


;; ============================================================================
;; Integration tests (through the full app)
;; ============================================================================

(deftest test-app-datasets-endpoint
  (testing "Full app handles /api/datasets"
    (let [response (routes/app {:request-method :get
                                :uri "/api/datasets"})]
      (is (= 200 (:status response))))))


(deftest test-app-root-page
  (testing "Full app handles root page"
    (let [response (routes/app {:request-method :get
                                :uri "/"})]
      ;; Should return 200 for the index page
      (is (= 200 (:status response))))))
