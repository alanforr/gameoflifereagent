(require '[cljs.build.api :as api]
         '[clojure.java.shell :as shell]
         '[clojure.string :as string])

;;; Configuration.

(def source-dir "src")

(def test-dir "test")

(def compiler-config {:main          'gameoflife.core
                      :output-to     "target/gameoflife/main.js"
                      :output-dir    "target/gameoflife/main"
                      :optimizations :simple
                      :source-map    "target/gameoflife/main.js.map"})

(def test-config {:main          'gameoflife.test-runner
                  :output-to     "target/test.js"
                  :output-dir    "target/test"
                  :optimizations :none
                  :source-map    true})

(def test-environment {:SOME_ENV_VAR "some-env-value"})

(def dev-config (merge compiler-config
                       {:optimizations :none
                        :source-map    true}))


;;; Tasks mechanism.

(defmulti task first)

(defmethod task :default
  [args]
  (let [all-tasks (-> task methods (dissoc :default) keys sort (->> (interpose ", ") (apply str)))]
    (println "unknown or missing task argument. Choose one of:" all-tasks)
    (System/exit 1)))


;;; Helper functions.

(defn try-require [ns-sym]
  (try (require ns-sym) true (catch Exception e false)))

(defmacro with-namespaces
  [namespaces & body]
  (if (every? try-require namespaces)
    `(do ~@body)
    `(do (println "task not available - required dependencies not found")
         (System/exit 1))))


;;; Compiling task.

(defn compile-once []
  (api/build source-dir compiler-config))

(defn compile-refresh []
  (api/watch source-dir compiler-config))

(defmethod task "compile" [[_ type]]
  (case type
    (nil "once") (compile-once)
    "watch"      (compile-refresh)
    (do (println "Unknown argument to compile task:" type)
        (System/exit 1))))

;;; Figwheeling task

(defmethod task "figwheel" [[_ port]]
  (with-namespaces [figwheel-sidecar.repl-api]
    (figwheel-sidecar.repl-api/start-figwheel!
     {:figwheel-options (cond-> {}
                          port (merge {:nrepl-port       (some-> port Long/parseLong)
                                       :nrepl-middleware ["cemerick.piggieback/wrap-cljs-repl"]}))
      :all-builds       [{:id           "dev"
                          :figwheel     true
                          :source-paths [source-dir]
                          :compiler     dev-config}]})
    (when-not port
      (figwheel-sidecar.repl-api/cljs-repl))))


;;; Build script entrypoint.

(task *command-line-args*)
