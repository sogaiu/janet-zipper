# XXX

# "without moving" property
# * insert-child
# * append-child

# scan for similar claims and enumerate here

# XXX: figure out how to generate random trees

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

  (= (length a-zip)
     2)
  # => true

  (deep= a-node
         (z/node a-zip))
  # => true

  (dictionary? (z/state a-zip))
  # => true

  (deep=
    (sort (keys (table/getproto (a-zip 1))))
    (sort @[:branch? :children :make-node :make-state]))
  # => true

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
  # => true

  )
