(def project 'elle)
(def version "0.1.0-SNAPSHOT")

(boot.core/set-env! :resource-paths #{"resources" "src"}
          :source-paths   #{"test"}
          :dependencies   '[[org.clojure/clojure "1.9.0"]
                            [instaparse "1.4.9"]
                            [adzerk/boot-test "RELEASE" :scope "test"]])

(task-options!
 aot {:namespace   #{'elle.core}}
 pom {:project     project
      :version     version
      :description "FIXME: write description"
      :url         "http://example/FIXME"
      :scm         {:url "https://github.com/yourname/elle"}
      :license     {"Eclipse Public License"
                    "http://www.eclipse.org/legal/epl-v10.html"}}
 repl {:init-ns    'elle.core}
 jar {:main        'elle.core
      :file        (str "elle-" version "-standalone.jar")})

(deftask build
  "Build the project locally as a JAR."
  [d dir PATH #{str} "the set of directories to write to (target)."]
  (let [dir (if (seq dir) dir #{"target"})]
    (comp (aot) (pom) (uber) (jar) (target :dir dir))))

(deftask run
  "Run the project."
  [a args ARG [str] "the arguments for the application."]
  (with-pass-thru fs
    (require '[elle.core :as app])
    (apply (resolve 'app/-main) args)))

(require '[adzerk.boot-test :refer [test]])
