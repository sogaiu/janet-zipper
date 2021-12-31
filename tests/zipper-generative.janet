# XXX

# "without moving" property
# * insert-child
# * append-child

# scan for similar claims and enumerate here

# * figure out how to generate random trees
#   * one idea
#     * choose n for length of top-level tuple
#     * fill in each of the n spots with either:
#       * tuple (recurse?)
#       * keyword / symbol / number

# XXX: figure out how to generate random sequence of operations?

(import ../janet-zipper/zipper :as z)

# zipper
(comment

  )

# zip
(comment

  (def a-node
    [:x [:y :z]])
  
  (def a-zip
    (z/zip a-node))

  (length a-zip)
  # =>
  2

  a-node
  # =>
  (z/node a-zip)

  (dictionary? (z/state a-zip))
  # =>
  true

  (sort (keys (table/getproto (a-zip 1))))
  # =>
  (sort @[:branch? :children :make-node :make-state])

  )

# node
(comment

  )

# state
(comment

  )

# branch?
(comment

  )

# children
(comment

  )

# make-state
(comment

  (let [a-zip (z/zip [:a :b [:x :y]])
        pt (table/getproto (z/make-state a-zip))]
    (all |(= :function
             (type (pt $)))
         [:branch? :children :make-node :make-state]))
  # =>
  true

  )
