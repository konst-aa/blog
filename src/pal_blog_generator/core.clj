(ns pal-blog-generator.core
  (:gen-class))

(use 'selmer.parser)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (render "hello world!" {})))
