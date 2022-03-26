(ns build
  (:require
   [clojure.tools.build.api :as b]
   [clojure.java.shell :as shell]
   [clojure.string :as str]
   [meander.epsilon :as m]
   [deps-deploy.deps-deploy :as d]))

(def scm-url "git@github.com:ribelo/doxa.git")

(defn sha
  [{:keys [dir path] :or {dir "."}}]
  (-> {:command-args (cond-> ["git" "rev-parse" "HEAD"]
                       path (conj "--" path))
       :dir (.getPath (b/resolve-path dir))
       :out :capture}
      b/process
      :out
      str/trim))

(defn git-branch-name
  "Attempts to get the current branch name via the shell."
  []
  (m/match (shell/sh "git" "rev-parse" "--abbrev-ref" "HEAD")
    {:exit 0, :out ?out}
    (str/trim ?out)

    ?result
    (throw (ex-info "Unable to compute branch name" ?result))))

(def git-commit-count-start
  "Starting SHA to count commits from."
  "fdf3f641f17c00ff3a3103cccaf9787bd14fa54b")

(defn git-branch-commit-count
  "Attempts to get the current number of commits on the current branch
  via the shell."
  []
  (m/match (shell/sh "git" "rev-list" (str git-commit-count-start "...") "--count")
    {:exit 0, :out ?out}
    (str/trim ?out)

    ?result
    (throw (ex-info "Unable to compute commit count" ?result))))

(def lib (symbol "com.github.ribelo" "doxa"))
(def basis (b/create-basis {:project "deps.edn"}))
(def version (format "0.1.%s" (git-branch-commit-count)))
(def class-dir "target/classes")
(def uber-file (format "target/%s-%s.jar" (git-branch-name) version))

(defn uber [_]
  (b/delete {:path "target"})
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs ["src/main"]
                :scm {:tag (sha nil)
                      :connection (str "scm:git:" scm-url)
                      :developerConnection (str "scm:git:" scm-url)
                      :url scm-url}})
  (b/copy-dir {:src-dirs   ["src/main"]
               :target-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis}))

(defn deploy [args]
  (-> args
      (assoc :artifact uber-file
             :pom-file (str class-dir "/META-INF/maven/" (namespace lib) "/" (name lib) "/pom.xml"))
      (d/deploy)))

(defn clojars [args]
  (-> {:installer :remote :sign-releases? true}
      (merge args)
      (deploy)))

(defn install [args]
  (-> {:installer :local}
      (merge args)
      (deploy)))
