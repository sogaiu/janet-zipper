# based on code by corasaurus-hex

# notes
#
# * "zipper" and "location" appear to mean the same thing.  do they?
#
# * "node" and "item" appear to mean the same thing too.
#
# * is it worth carrying around the 3 functions (branch?, children,
#   and make-node) or should they just be made external?
#
# * "rightmost" or "remove" might be broken?

# XXX: differs from clojure's behavior
#      e.g. (butlast [:a]) would yield nil(?!) in clojure
(defn butlast
  [indexed]
  (if (empty? indexed)
    nil
    (let [new-indexed (array/slice indexed 0 -2)]
      (if (tuple? indexed)
        (tuple ;new-indexed)
        new-indexed))))

(comment

  (butlast [:a :b :c])
  # => [:a :b]

  (butlast [:a])
  # => []

  (butlast [])
  # => nil

  (butlast @[:a :b :c])
  # => @[:a :b]

  (butlast @[:a])
  # => @[]

  (butlast @[])
  # => nil

  )

(defn rest
  [indexed]
  (if (empty? indexed)
    nil
    (let [new-indexed (array/slice indexed 1 -1)]
      (if (tuple? indexed)
        (tuple ;new-indexed)
        new-indexed))))

(comment

  (rest [:a :b :c])
  # => [:b :c]

  (rest [:a :b])
  # => [:b]

  (rest [:a])
  # => []

  (rest [])
  # => nil

  (rest @[:a :b :c])
  # => @[:b :c]

  (rest @[:a :b])
  # => @[:b]

  (rest @[:a])
  # => @[]

  (rest @[])
  # => nil

  )

(defn tuple-push
  [tup x & xs]
  (if tup
    [;tup x ;xs]
    [x ;xs]))

(comment

  (tuple-push [:a :b] :c)
  # => [:a :b :c]

  (tuple-push nil :a)
  # => [:a]

  (tuple-push [:a] :b :c)
  # => [:a :b :c]

  (tuple-push [] :a)
  # => [:a]

  )

(defn to-entries
  [val]
  (if (dictionary? val)
    (pairs val)
    val))

(comment

  (to-entries {:a 1 :b 2})
  # => @[[:a 1] [:b 2]]

  (to-entries {:a 1})
  # => @[[:a 1]]

  (to-entries {})
  # => @[]

  (to-entries @{:a 1 :b 2})
  # => @[[:a 1] [:b 2]]

  (to-entries @{:a 1})
  # => @[[:a 1]]

  (to-entries @{})
  # => @[]

  # XXX: leaving non-dictionaries alone and passing through...
  #      is this desirable over erroring?
  (to-entries [:a :b :c])
  # => [:a :b :c]

  )

# XXX: when xs is empty, "all" becomes nil
(defn first-rest-maybe-all
  [xs]
  (if (or (nil? xs) (empty? xs))
    [nil nil nil]
    [(first xs) (rest xs) xs]))

(comment

  (first-rest-maybe-all [:a])
  # => [:a [] [:a]]

  (first-rest-maybe-all [:a :b])
  # => [:a [:b] [:a :b]]

  (first-rest-maybe-all @[:a])
  # => [:a @[] @[:a]]

  (first-rest-maybe-all @[:a :b])
  # => [:a @[:b] @[:a :b]]

  (first-rest-maybe-all [])
  # => [nil nil nil]

  # XXX: is this what we want?
  (first-rest-maybe-all nil)
  # => [nil nil nil]

  )

(defn zipper
  ``
  Returns a new zipper consisting of three elements:

  * `root` - the passed in root node.

  * `state` - struct of info about node's location in the tree with keys:

    * `:ls` - left siblings

    * `:pnodes` - path of nodes from root to current location

    * `:pstate` - parent node's state

    * `:rs` - right siblings

    * `:changed?` - indicates whether "editing" has occured

  * `fns` - struct of the three functions passed in:

    * :branch? - fn that tests if a node is a branch (has children)

    * :children - fn that returns the child nodes for the given branch.

    * :make-node - fn that takes a node + children and returns a new branch
                   node with the same.
  ``
  [root &keys {:branch? branch?
               :children children
               :make-node make-node}]
  [root
   nil
   {:branch? branch?
    :children children
    :make-node make-node}])

(defn zip
  ``
  Returns a zipper for nested sequences (tuple/array/table/struct),
  given a root sequence.
  ``
  [sequence]
  (zipper sequence
          :branch? |(or (dictionary? $) (indexed? $))
          :children to-entries
          :make-node (fn [p xs] xs)))

(comment

  (let [a-zip (zip [:x [:y :z]])]
    (= 3 (length a-zip)))
  # => true

  (let [a-tuple [:x [:y :z]]
        a-zip (zip a-tuple)]
    (deep= a-tuple
           (a-zip 0)))
  # => true

  (let [a-zip (zip [:x [:y :z]])]
    (dictionary? (a-zip 2)))
  # => true

  (let [a-zip (zip [:x [:y :z]])]
    (deep=
      (keys (a-zip 2))
      @[:branch? :make-node :children]))
  # => true

  )

(defn node
  "Returns the node at `location`."
  [location]
  (location 0))

(comment

  (node (zip [:a :b [:x :y]]))
  # => [:a :b [:x :y]]

  )

(defn fns
  "Returns the fns for `location`."
  [location]
  (location 2))

(comment

  (def c-zip
    (zip [:a :b [:x :y]]))

  (= to-entries
    ((fns c-zip) :children))
  # => true

  (function? ((fns c-zip) :branch?))
  # => true

  (function? ((fns c-zip) :make-node))
  # => true

  )

(defn branch?
  ``
  Returns true if the node at `location` is a branch.
  Returns false otherwise.
  ``
  [location]
  (((fns location) :branch?) (node location)))

(comment

  (branch? (zip [:a :b [:x :y]]))
  # => true

  )

(defn children
  ``
  Gets the children of a branch node at `location`.
  Raises an error if `location` is a leaf.
  ``
  [location]
  (if (branch? location)
    (((fns location) :children) (node location))
    (error "called children on a leaf node")))

(comment

  (children (zip [:a :b [:x :y]]))
  # => [:a :b [:x :y]]

  )

(defn down
  ``
  Moves down the tree, returning the leftmost child location of
  `location`, or nil if there are no children.
  ``
  [location]
  (when (branch? location)
    (let [[node st _] location
          [k rest-kids kids]
          (first-rest-maybe-all (children location))]
      (when kids
        [k
         @{:ls []
           :pnodes (if st
                     (tuple-push (st :pnodes) node)
                     [node])
           :pstate st
           :rs rest-kids}
         (fns location)]))))

(comment

  (node (down (zip [:a :b [:x :y]])))
  # => :a

  (-> (zip [:a :b [:x :y]])
      down
      branch?)
  # => false

  )

(defn make-node
  ``
  Convenience function for calling the :make-node fn from the fns at
  `location`.
  ``
  [location node children]
  (((fns location) :make-node) node children))

(comment

  (def f-zip
    (zip [:a :b [:x :y]]))

  (make-node f-zip
             [:a :b] [:x :y])
  # => [:x :y]

  )

(defn state
  "Returns the state for the given location."
  [location]
  (location 1))

(comment

  (state (zip [:a :b [:x :y]]))
  # => nil

  (deep=
    #
    (-> (zip [:a :b [:x :y]])
        down
        state)
    #
    '@{:ls ()
       :pnodes ((:a :b (:x :y)))
       :rs (:b (:x :y))})
  # => true

  (deep=
    #
    (-> (zip [[:a [:b [:x :y]]]])
        down
        down
        right
        state)
    #
    '@{:ls (:a)
       :pnodes (((:a (:b (:x :y)))) (:a (:b (:x :y))))
       :pstate @{:ls ()
                :pnodes (((:a (:b (:x :y)))))
                :rs ()}
       :rs ()})
  # => true

  )

(defn path
  "Returns the sequence of nodes that lead to this location."
  [location]
  (when-let [st (state location)]
    (st :pnodes)))

(comment

  (path (zip [:a :b [:x :y]]))
  # => nil

  (-> (zip [:a :b [:x :y]])
      down
      path)
  # => [[:a :b [:x :y]]]

  )

(defn right
  [location]
  (let [[node st the-fns] location
        {:ls ls :rs rs} (or st @{})
        [r rest-rs rs] (first-rest-maybe-all rs)]
    (when (and st rs)
      [r
       (merge st
              {:ls (tuple-push ls node)
               :rs rest-rs})
       the-fns])))

(comment

  (right (down (zip [:a])))
  # => nil

  (-> (zip [:a :b [:x :y]])
      down
      right
      node)
  # => :b

  (-> (zip [:a :b [:x :y]])
      down
      right
      right
      down
      node)
  # => :x

  (-> (zip [:a :b [:x :y]])
      down
      right
      right
      down
      path)
  # => [[:a :b [:x :y]] [:x :y]]

  (-> (zip [:a :b [:x :y]])
      down
      right
      right
      branch?)
  # => true

  )

(defn left
  [location]
  (let [[node st the-fns] location
        {:ls ls :rs rs} (or st @{})]
    (when (and st
               (indexed? ls)
               (not (empty? ls)))
      [(last ls)
       (merge st {:ls (butlast ls)
                  :rs [node ;rs]})
       the-fns])))

(comment

  (left (down (zip [:a])))
  # => nil

  (-> (zip [:a :b :c])
      down
      right
      right
      left
      node)
  # => :b

  (let [a-zip
        (zip [:a :b [:x :y]])]
    (deep=
      (down a-zip)
      (left (right (down a-zip)))))
  # => true

  )

(defn up
  ``
  Moves up the tree, returning the parent location of `location`,
  or nil if at the root.
  ``
  [location]
  (let [[node st the-fns] location
        {:ls ls
         :pnodes pnodes
         :pstate pstate
         :rs rs
         :changed? changed?}
        (or st @{})]
    (when pnodes
      (let [pnode (last pnodes)]
        (if changed?
          [(make-node location pnode [;ls node ;rs])
           (and pstate (merge pstate {:changed? true}))
           the-fns]
          [pnode
           pstate
           the-fns])))))

(comment

  (def m-zip
    (zip [:a :b [:x :y]]))

  (deep=
    (-> m-zip
        down
        up)
    m-zip)
  # => true

  (deep=
    (-> m-zip
        down
        right
        left
        up)
    m-zip)
  # => true

  )

(defn lefts
  [location]
  (if-let [st (state location)
           ls (st :ls)]
    ls
    []))

(comment

  (-> (zip [:a :b])
      down
      lefts)
  # => []

  (-> (zip [:a :b])
      down
      right
      lefts)
  # => [:a]

  (-> (zip [:a :b [:x :y]])
      down
      right
      right
      lefts)
  # => [:a :b]

  (-> (zip [:a :b [:x :y]])
      down
      right
      right
      down
      right
      lefts)
  # => [:x]

  )

(defn rights
  "Returns the sequence of siblings to the right of `location`."
  [location]
  (when-let [st (state location)]
    (st :rs)))

(comment

  (-> (zip [:a :b [:x :y]])
      down
      rights)
  # => [:b [:x :y]]

  )

(defn end?
  "Returns true if `location` represents the end of a depth-first walk."
  [location]
  (= :end (state location)))

(defn root
  ``
  Moves all the way up the tree for `location` and returns the node at
  the root location.
  ``
  [location]
  (if (end? location)
    (node location)
    (if-let [p (up location)]
      (root p)
      (node location))))

(comment

  (let [a-zip
        (zip [:a :b [:x :y]])]
    (deep=
      (node a-zip)
      (-> a-zip
          down
          right
          right
          down
          root)))
    # => true

  )

(defn df-next
  ``
  Moves to the next location in the hierarchy, depth-first.  When
  reaching the end, returns a distinguished location detectable
  via `end?`.  If already at the end, stays there.
  ``
  [location]
  (defn recur
    [loc]
    (if (up loc)
      (or (right (up loc))
          (recur (up loc)))
      [(node loc) :end]))
  (if (end? location)
    location
    (or (and (branch? location) (down location))
        (right location)
        (recur location))))

(comment

  (def n-zip
    (zip [:a :b [:x]]))

  (node (df-next n-zip))
  # => :a

  (-> n-zip
      df-next
      df-next
      node)
  # => :b

  (-> n-zip
      df-next
      df-next
      df-next
      node)
  # => [:x]

  (-> n-zip
      df-next
      df-next
      df-next
      df-next
      node)
  # => :x

  (-> n-zip
      df-next
      df-next
      df-next
      df-next
      df-next
      node)
  # => [:a :b [:x]]

  (-> n-zip
      df-next
      df-next
      df-next
      df-next
      df-next
      end?)
  # => true

  )

(defn rightmost
  ``
  Returns the location of the rightmost sibling of the node at
  `location`, or the current node if there are none to the right.
  ``
  [location]
  (let [[node st the-fns] location
        {:ls ls :rs rs} (or st @{})]
    (if (and st
             (indexed? rs)
             (not (empty? rs)))
      [(last rs)
       (merge st {:ls (tuple-push ls node ;(butlast rs))
                  :rs []})
       the-fns]
      location)))

(comment

  (-> (zip [:a :b [:x :y]])
      down
      rightmost
      node)
  # => [:x :y]

  (-> (zip [:a :b [:x :y]])
      down
      right
      right
      rightmost
      node)
  # => [:x :y]

  (-> (zip [:a :b [:x :y]])
      down
      right
      right
      down
      rightmost
      node)
  # => :y

  (-> (zip [:a :b [:x :y]])
      down
      right
      right
      down
      right
      rightmost
      node)
  # => :y

  )

(defn leftmost
  ``
  Returns the location of the leftmost sibling of the node at `location`,
  or the current node if there are no siblings to the left.
  ``
  [location]
  (let [[node st the-fns] location
        {:ls ls :rs rs} (or st @{})]
    (if (and st
             (indexed? ls)
             (not (empty? ls)))
      [(first ls)
       (merge st {:ls []
                  :rs [;(rest ls) node ;rs]})
       the-fns]
      location)))

(comment

  (def q-zip
    (zip [:a :b [:x :y]]))

  (-> q-zip
      down
      leftmost
      node)
  # => :a

  (-> q-zip
      down
      right
      leftmost
      node)
  # => :a

  (-> q-zip
      down
      right
      right
      down
      leftmost
      node)
  # => :x

  )

(defn insert-left
  ``
  Inserts `item` as the left sibling of the node at `location`,
  without moving.
  ``
  [location item]
  (let [[node st the-fns] location
        {:ls ls} (or st @{})]
    (if st
      [node
       (merge st {:ls (tuple-push ls item)
                  :changed? true})
       the-fns]
      (error "Insert at top"))))

(comment

  (def r-zip
    (zip [:a :b [:x :y]]))

  (root (insert-left (down r-zip) :z))
  # => [:z :a :b [:x :y]]

  (try
    (insert-left r-zip :e)
    ([e] e))
  # => "Insert at top"

  )

(defn insert-right
  ``
  Inserts `item` as the right sibling of the node at `location`,
  without moving.
  ``
  [location item]
  (let [[node st the-fns] location
        {:rs rs} (or st @{})]
    (if st
      [node
       (merge st {:rs [item ;rs]
                  :changed? true})
       the-fns]
      (error "Insert at top"))))

(comment

  (def s-zip
    (zip [:a :b [:x :y]]))

  (-> s-zip
      down
      (insert-right :z)
      root)
  # => [:a :z :b [:x :y]]

  (try
    (insert-right s-zip :e)
    ([e] e))
  # => "Insert at top"

  )

(defn replace
  "Replaces existing node at `location` with `node`, without moving."
  [location node]
  (let [[_ st the-fns] location
        st (or st @{})]
    [node
     (merge st {:changed? true})
     the-fns]))

(comment

  (-> (zip [:a :b [:x :y]])
      down
      (replace :w)
      root)
  # => [:w :b [:x :y]]

  (-> (zip [:a :b [:x :y]])
      down
      right
      right
      down
      (replace :w)
      root)
  # => [:a :b [:w :y]]

  )

(defn edit
  "Replaces the node at `location` with the value of `(f node args)`."
  [location f & args]
  (replace location
           (apply f (node location) args)))

(comment

  (-> (zip [1 2 [8 9]])
      down
      (edit inc)
      root)
  # => [2 2 [8 9]]

  (-> (zip [1 2 [8 9]])
      down
      right
      right
      down
      right
      (edit dec)
      root)
  # => [1 2 [8 8]]

  (-> (zip [1 2 [8 9]])
      down
      right
      right
      down
      (edit + 2)
      root)
  # => [1 2 [10 9]]

  )

(defn insert-child
  ``
  Inserts `item` as the leftmost child of the node at `location`,
  without moving.
  ``
  [location item]
  (replace location
           (make-node location
                      (node location)
                      [item ;(children location)])))

(comment

  (def v-zip
    (zip [:a :b [:x :y]]))

  (root (insert-child v-zip :c))
  # => [:c :a :b [:x :y]]

  (-> v-zip
      down
      right
      right
      (insert-child :c)
      root)
  # => [:a :b [:c :x :y]]

  )

(defn append-child
  ``
  Appends `item` as the rightmost child of the node at `location`,
  without moving.
  ``
  [location item]
  (replace location
           (make-node location
                      (node location)
                      [;(children location) item])))

(comment

  (def w-zip
    (zip [:a :b [:x :y]]))

  (root (append-child w-zip :c))
  # => [:a :b [:x :y] :c]

  (-> w-zip
      down
      right
      right
      (append-child :c)
      root)
  # => [:a :b [:x :y :c]]

  )

(defn df-prev
  ``
  Moves to the previous location in the hierarchy, depth-first.
  If already at the root, returns nil.
  ``
  [location]
  #
  (defn recur
    [loc]
    (if-let [child (and (branch? loc)
                        (down loc))]
      (recur (rightmost child))
      loc))
  #
  (if-let [left-loc (left location)]
    (recur left-loc)
    (up location)))

(comment

  (def x-zip
    (zip [:a :b [:x :y]]))

  (-> x-zip
      down
      right
      df-prev
      node)
  # => :a

  (-> x-zip
      down
      right
      right
      df-prev
      node)
  # => :b

  (-> x-zip
      down
      right
      right
      down
      df-prev
      node)
  # => [:x :y]

  (-> x-zip
      down
      right
      right
      down
      df-prev
      df-prev
      node)
  # => :b

  )

(defn remove
  [location]
  ``
  Removes the node at `location`, returning the location that would have
  preceded it in a depth-first walk.
  ``
  [location]
  (let [[node st the-fns] location
        {:ls ls
         :pnodes pnodes
         :pstate pstate
         :rs rs}
        (or st @{})]
    #
    (defn recur
      [loc]
      (if-let [child (and (branch? loc) (down loc))]
        (recur (rightmost child))
        loc))
    #
    (if st
      (if (pos? (length ls))
        (recur [(last ls)
                (merge st {:ls (butlast ls)
                           :changed? true})
                the-fns])
        [(make-node location (last pnodes) rs)
         (and pstate (merge pstate {:changed? true}))
         the-fns])
      (error "Remove at top"))))

(comment

  (def y-zip
    (zip [:a :b [:x :y]]))

  (-> y-zip
      down
      right
      remove
      node)
  # => :a

  (-> y-zip
      down
      right
      remove
      root)
  # => [:a [:x :y]]

  (-> y-zip
      down
      right
      right
      remove
      root)
  # => [:a :b]

  (try
    (remove y-zip)
    ([e] e))
  # => "Remove at top"

  )
