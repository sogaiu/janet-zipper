(import ../janet-zipper/zipper :as z)

# zipper
(comment

  )

# zip
(comment

  (def a-node
    [:a [:b [:x :y]]])

  (deep= (z/zip a-node)
         [a-node @{}])
  # => true

  )

# node
(comment

  (def a-node
    [:a [:b [:x :y]]])

  (deep= (z/node (z/zip a-node))
         a-node)
  # => true

  )

# state
(comment

  # merge is used to "remove" the prototype table of `st`
  (merge {}
         (z/state (z/zip [:a :b [:x :y]])))
  # => @{}

  (deep=
    #
    (merge {}
           (-> (z/zip [:a :b [:x :y]])
               z/down
               z/state))
    #
    '@{:ls ()
       :pnodes ((:a :b (:x :y)))
       :pstate @{}
       :rs (:b (:x :y))})
  # => true

  (deep=
    #
    (merge {}
           (-> (z/zip [[:a [:b [:x :y]]]])
               z/down
               z/down
               z/right
               z/state))
    #
    '@{:ls (:a)
       :pnodes (((:a (:b (:x :y)))) (:a (:b (:x :y))))
       :pstate @{:ls ()
                 :pnodes (((:a (:b (:x :y)))))
                 :pstate @{}
                 :rs ()}
       :rs ()})
  # => true

  )

# branch?
(comment

  (def a-zip
    (z/zip [:a [:b [:x :y]]]))

  (z/branch? a-zip)
  # => true

  (-> a-zip
      z/down
      z/right
      z/branch?)
  # => true

  (-> a-zip
      z/down
      z/right
      z/down
      z/branch?)
  # => false

  (-> a-zip
      z/down
      z/right
      z/down
      z/right
      z/branch?)
  # => true

  )

# children
(comment

  (def a-zip
    (z/zip [:a [:b [:x :y]]]))

  (-> a-zip
      z/children
      first)
  # => :a

  (-> a-zip
      z/down
      z/right
      z/down
      z/right
      z/children)
  # => [:x :y]

  )

# make-state
(comment

  )

# down
(comment

  (-> (z/zip [:a])
      z/down
      z/node)
  # => :a

  (-> (z/zip [[[:a :b] :c]])
      z/down
      z/down
      z/children)
  # => [:a :b]

  )

# right
(comment

  (-> (z/zip [:a :b [:x :y]])
      z/down
      z/right
      z/right
      z/down
      z/node)
  # => :x

  (-> (z/zip [:a :b [:x :y]])
      z/down
      z/right
      z/right
      z/branch?)
  # => true

  (-> [:code
       [:tuple
        [:number "1"] [:whitespace " "]
        [:number "2"]]]
      z/zip
      z/down
      z/right
      z/down
      z/right
      z/right
      z/node)
  # => [:whitespace " "]

  )

# make-node
(comment

  # users of make-node:
  #
  # * up
  # * remove
  # * append-child
  # * insert-child

  )

# up
(comment

  (def a-zip
    (z/zip [[[:a :b :c] :d] :e]))
  
  (-> a-zip
      z/down
      z/down
      z/down
      z/right
      z/right
      z/up
      z/right
      z/up
      z/right
      z/up
      z/node)
  # => (z/node a-zip)

  )

# end?
(comment

  # users of end?:
  #
  # * root
  # * df-next

  )

# # root
# (comment

#   (def code
#     ``
#     (+ 1 2)
#     ``)

#   (-> code
#       r/ast
#       z/zip-down
#       z/down
#       z/right-skip-wsc
#       z/right-skip-wsc
#       z/root
#       r/code
#       (= code))
#   # => true

#   )

# # df-next, end?
# (comment

#   (def n-zip
#     (zip [:a :b [:x]]))

#   (-> n-zip
#       df-next
#       df-next
#       df-next
#       node)
#   # => [:x]

#   (-> n-zip
#       df-next
#       df-next
#       df-next
#       df-next
#       node)
#   # => :x

#   (-> n-zip
#       df-next
#       df-next
#       df-next
#       df-next
#       df-next
#       node)
#   # => [:a :b [:x]]

#   )

# # rightmost
# (comment

#   (-> (z/zip [:a :b [:x :y]])
#       z/down
#       z/right
#       z/right
#       z/rightmost
#       z/node)
#   # => [:x :y]

#   (-> (z/zip [:a :b [:x :y]])
#       z/down
#       z/right
#       z/right
#       z/down
#       z/rightmost
#       z/node)
#   # => :y

#   (-> (z/zip [:a :b [:x :y]])
#       z/down
#       z/right
#       z/right
#       z/down
#       z/right
#       z/rightmost
#       z/node)
#   # => :y

#   (-> (z/zip ['def 'm {:a 1 :b 2}])
#       z/down
#       z/rightmost
#       z/down
#       z/rightmost
#       z/node)
#   # => [:b 2]

#   (-> (z/zip ['def 'm [:a 1 :b 2]])
#       z/down
#       z/rightmost
#       z/down
#       z/rightmost
#       z/node)
#   # => 2

#   )

# # replace
# (comment

#   (-> "(+ 1 (/ 2 3))"
#       r/ast
#       z/zip-down
#       z/down
#       z/rightmost
#       z/down
#       z/rightmost
#       (z/replace [:number "8"])
#       z/root
#       r/code)
#   # => "(+ 1 (/ 2 8))"

#   )

# # edit
# (comment

#   (-> (zip [1 2 [8 9]])
#       down
#       right
#       right
#       down
#       right
#       (edit dec)
#       root)
#   # => [1 2 [8 8]]

#   (-> (zip [1 2 [8 9]])
#       down
#       right
#       right
#       down
#       (edit + 2)
#       root)
#   # => [1 2 [10 9]]

#   )

# # search
# (comment

#   (-> "(+ 1 2)"
#       r/ast
#       z/zip
#       (z/search |(match (z/node $)
#                    [:number "1"]
#                    true))
#       (z/edit |(match $
#                  [:number num-str]
#                  [:number (-> num-str
#                               scan-number
#                               inc
#                               string)]))
#       z/root
#       r/code)
#   # => "(+ 2 2)"

#   )

# # remove
# (comment

#   (-> (z/zip [:a :b [:x :y]])
#       z/down
#       z/right
#       z/remove
#       z/root)
#   # => [:a [:x :y]]

#   (-> (z/zip [:a :b [:x :y]])
#       z/down
#       z/right
#       z/right
#       z/remove
#       z/root)
#   # => [:a :b]

#   )

# # append-child
# (comment

#   (-> (z/zip [:a :b [:x :y]])
#       z/down
#       z/right
#       z/right
#       (z/append-child :c)
#       z/root)
#   # => [:a :b [:x :y :c]]

#   )

# # insert-child
# (comment

#   (-> (z/zip [:a :b [:x :y]])
#       z/down
#       z/right
#       z/right
#       (z/insert-child :c)
#       z/root)
#   # => [:a :b [:c :x :y]]

#   )

# # insert-right
# (comment

#   (-> "(defn my-fn [x] (+ x y))"
#       r/ast
#       z/zip-down
#       z/down
#       (z/search |(match (z/node $)
#                    [:bracket-tuple [:symbol "x"]]
#                    true))
#       z/down
#       (z/insert-right [:symbol "y"])
#       (z/insert-right [:whitespace " "])
#       z/root
#       r/code)
#   # => "(defn my-fn [x y] (+ x y))"

#   (try
#     (-> "(+ 1 3)"
#         r/ast
#         z/zip
#         (z/insert-right [:keyword ":oops"]))
#     ([e] e))
#   # => "Called `insert-right` at root"

#   )

# # insert-left
# (comment

#   (-> "(a 1)"
#       r/ast
#       z/zip-down
#       z/down
#       (z/insert-left [:symbol "def"])
#       (z/insert-left [:whitespace " "])
#       z/root
#       r/code)
#   # => "(def a 1)"

#   (try
#     (-> "(/ 8 9)"
#         r/ast
#         z/zip
#         (z/insert-left [:keyword ":oops"]))
#     ([e] e))
#   # => "Called `insert-left` at root"

#   )

# # lefts
# (comment

#   (-> (z/zip [:a :b])
#       z/down
#       z/right
#       z/lefts)
#   # => [:a]

#   (-> (z/zip [:a :b [:x :y]])
#       z/down
#       z/right
#       z/right
#       z/down
#       z/right
#       z/lefts)
#   # => [:x]

#   )

# # rights
# (comment

#   (deep=
#     #
#     (-> "(+ (- 8 1) 2)"
#         r/ast
#         z/zip-down
#         z/down
#         z/right-skip-wsc
#         z/down
#         z/rights)
#     #
#     '[(:whitespace " ")
#       (:number "8") (:whitespace " ")
#       (:number "1")])
#   # => true

#   )

# # df-prev
# (comment

#   (-> [:a :b [:x :y]]
#       z/down
#       z/right
#       z/right
#       z/df-prev
#       z/node)
#   # => :b

#   (-> [:a :b [:x :y]]
#       z/down
#       z/right
#       z/right
#       z/down
#       z/df-prev
#       z/df-prev
#       z/node)
#   # => :b

#   )

# # leftmost
# (comment

#   (-> (z/zip [:a :b [:x :y]])
#       z/down
#       z/right
#       z/right
#       z/down
#       z/leftmost
#       z/node)
#   # => :x

#   )

# # path
# (comment

#   (deep=
#     #
#     (-> "(+ (/ 3 8) 2)"
#         r/ast
#         z/zip-down
#         z/down
#         z/right-skip-wsc
#         z/down
#         z/path)
#     #
#     '(@[:code
#         (:tuple
#           (:symbol "+") (:whitespace " ")
#           (:tuple
#             (:symbol "/") (:whitespace " ")
#             (:number "3") (:whitespace " ")
#             (:number "8"))
#           (:whitespace " ") (:number "2"))]
#        (:tuple
#          (:symbol "+") (:whitespace " ")
#          (:tuple
#            (:symbol "/") (:whitespace " ")
#            (:number "3") (:whitespace " ")
#            (:number "8"))
#          (:whitespace " ") (:number "2"))
#        (:tuple
#          (:symbol "/") (:whitespace " ")
#          (:number "3") (:whitespace " ")
#          (:number "8"))))
#   # => true

#   )

# # right-until
# (comment

#   (def code
#     ``
#     [1
#      # a comment
#      2]
#     ``)

#   (-> code
#       r/ast
#       z/zip-down
#       z/down
#       (z/right-until |(match (z/node $)
#                       [:comment _]
#                       false
#                       #
#                       [:whitespace _]
#                       false
#                       #
#                       true))
#       z/node)
#   # => [:number "2"]

#   (def code
#     ``
#     :skip-me

#     [1
#      # a comment
#      2]
#     ``)

#   (-> code
#       r/ast
#       z/zip-down
#       (z/right-until |(match (z/node $)
#                       [:comment _]
#                       false
#                       #
#                       [:whitespace _]
#                       false
#                       #
#                       true))
#       z/down
#       (z/right-until |(match (z/node $)
#                       [:comment _]
#                       false
#                       #
#                       [:whitespace _]
#                       false
#                       #
#                       true))
#       z/node)
#   # => [:number "2"]

#   )

# # left
# (comment

#   (def a-zip
#     (zip [:a :b [:x :y]]))

#   (deep=
#     (down a-zip)
#     (-> a-zip
#         down
#         right
#         left))
#   # => true

#   )

# # left-until
# (comment

#   (-> (r/ast
#         ``
#         {:a "hey"
#          :b "see"
#          :c "spot"
#          :x "sit"
#          :y "!?"}
#         ``)
#       z/zip-down
#       z/down
#       z/rightmost
#       (z/left-until |(match (z/node $)
#                        [:string]
#                        true))
#       z/node)
#   # => [:string `"sit"`]

#   )
