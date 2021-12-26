# based on code by corasaurus-hex

(import ./zip-support :as s)

(defn zipper
  ``
  Returns a new zipper consisting of two elements:

  * `root` - the passed in root node.

  * `state` - table of info about node's z-location in the tree with keys:

    * `:ls` - left siblings

    * `:pnodes` - path of nodes from root to current z-location

    * `:pstate` - parent node's state

    * `:rs` - right siblings

    * `:changed?` - indicates whether "editing" has occured

  * `state` has a prototype table with four functions:

    * :branch? - fn that tests if a node is a branch (has children)

    * :children - fn that returns the child nodes for the given branch.

    * :make-node - fn that takes a node + children and returns a new branch
                   node with the same.

    * :make-state - fn for creating a new state
  ``
  [root &keys {:branch? branch?
               :children children
               :make-node make-node}]
  #
  (defn make-state
    [&opt ls rs pnodes pstate changed?]
    (table/setproto @{:ls ls
                      :pnodes pnodes
                      :pstate pstate
                      :rs rs
                      :changed? changed?}
                    @{:branch? branch?
                      :children children
                      :make-node make-node
                      :make-state make-state}))
  #
  [root (make-state)])

(comment

  # XXX

  )

(defn zip
  ``
  Returns a zipper for nested sequences (tuple/array/table/struct),
  given a root sequence.
  ``
  [sequence]
  (zipper sequence
          :branch? |(or (dictionary? $) (indexed? $))
          :children s/to-entries
          :make-node (fn [p xs] xs)))

(comment

  (def a-node
    [:x [:y :z]])

  (def [the-node the-state]
    (zip a-node))

  (deep= the-node a-node)
  # => true

  # merge is used to "remove" the prototype table of `st`
  (deep= (merge {} the-state)
         @{})
  # => true

  )

(defn node
  "Returns the node at `zloc`."
  [zloc]
  (zloc 0))

(comment

  (node (zip [:a :b [:x :y]]))
  # => [:a :b [:x :y]]

  )

(defn state
  "Returns the state for `zloc`."
  [zloc]
  (zloc 1))

(comment

  # merge is used to "remove" the prototype table of `st`
  (merge {}
         (-> (zip [:a [:b [:x :y]]])
             state))
  # => @{}

  )

(defn branch?
  ``
  Returns true if the node at `zloc` is a branch.
  Returns false otherwise.
  ``
  [zloc]
  (((state zloc) :branch?) (node zloc)))

(comment

  (branch? (zip [:a :b [:x :y]]))
  # => true

  )

(defn children
  ``
  Returns children for a branch node at `zloc`.
  Otherwise throws an error.
  ``
  [zloc]
  (if (branch? zloc)
    (((state zloc) :children) (node zloc))
    (error "Called `children` on a non-branch zloc")))

(comment

 (children (zip [:a :b [:x :y]]))
  # => [:a :b [:x :y]]

  )

(defn make-state
  ``
  Convenience function for calling the :make-state function for `zloc`.
  ``
  [zloc &opt ls rs pnodes pstate changed?]
  (((state zloc) :make-state) ls rs pnodes pstate changed?))

(comment

  # merge is used to "remove" the prototype table of `st`
  (merge {}
         (make-state (zip [:a :b [:x :y]])))
  # => @{}

  )

(defn down
  ``
  Moves down the tree, returning the leftmost child z-location of
  `zloc`, or nil if there are no children.
  ``
  [zloc]
  (when (branch? zloc)
    (let [[node st] zloc
          [k rest-kids kids]
          (s/first-rest-maybe-all (children zloc))]
      (when kids
        [k
         (make-state zloc
                     []
                     rest-kids
                     (if (not (empty? st))
                       (s/tuple-push (st :pnodes) node)
                       [node])
                     st
                     (st :changed?))]))))

(comment

  (node (down (zip [:a :b [:x :y]])))
  # => :a

  (-> (zip [:a :b [:x :y]])
      down
      branch?)
  # => false

  (try
    (-> (zip [:a])
        down
        children)
    ([e] e))
  # => "Called `children` on a non-branch zloc"

  (deep=
    #
    (merge {}
           (-> [:a [:b [:x :y]]]
               zip
               down
               state))
    #
    '@{:ls ()
       :pnodes ((:a (:b (:x :y))))
       :pstate @{}
       :rs ((:b (:x :y)))})
  # => true

  )

(defn right
  ``
  Returns the z-location of the right sibling of the node
  at `zloc`, or nil if there is no such sibling.
  ``
  [zloc]
  (let [[node st] zloc
        {:ls ls :rs rs} st
        [r rest-rs rs] (s/first-rest-maybe-all rs)]
    (when (and (not (empty? st)) rs)
      [r
       (make-state zloc
                   (s/tuple-push ls node)
                   rest-rs
                   (st :pnodes)
                   (st :pstate)
                   (st :changed?))])))

(comment

  (-> (zip [:a :b])
      down
      right
      node)
  # => :b

  (-> (zip [:a])
      down
      right)
  # => nil

  )

(defn make-node
  ``
  Returns a branch node, given `zloc`, `node` and `children`.
  ``
  [zloc node children]
  (((state zloc) :make-node) node children))

(comment

  (make-node (zip [:a :b [:x :y]])
             [:a :b] [:x :y])
  # => [:x :y]

  )

(defn up
  ``
  Moves up the tree, returning the parent z-location of `zloc`,
  or nil if at the root z-location.
  ``
  [zloc]
  (let [[node st] zloc
        {:ls ls
         :pnodes pnodes
         :pstate pstate
         :rs rs
         :changed? changed?} st]
    (when pnodes
      (let [pnode (last pnodes)]
        (if changed?
          [(make-node zloc pnode [;ls node ;rs])
           (make-state zloc
                       (pstate :ls)
                       (pstate :rs)
                       (pstate :pnodes)
                       (pstate :pstate)
                       true)]
          [pnode pstate])))))

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
        right
        down
        up
        up)
    m-zip)
  # => true

  )

# XXX: used by `root` and `df-next`
(defn end?
  "Returns true if `zloc` represents the end of a depth-first walk."
  [zloc]
  (= :end (state zloc)))

(defn root
  ``
  Moves all the way up the tree for `zloc` and returns the node at
  the root z-location.
  ``
  [zloc]
  (if (end? zloc)
    (node zloc)
    (if-let [p (up zloc)]
      (root p)
      (node zloc))))

(comment

  (def a-zip
    (zip [:a :b [:x :y]]))

  (deep= (node a-zip)
         (-> a-zip
             down
             right
             right
             down
             root))
    # => true

  )

(defn df-next
  ``
  Moves to the next z-location, depth-first.  When the end is
  reached, returns a special z-location detectable via `end?`.
  Does not move if already at the end.
  ``
  [zloc]
  #
  (defn recur
    [loc]
    (if (up loc)
      (or (right (up loc))
          (recur (up loc)))
      [(node loc) :end]))
  #
  (if (end? zloc)
    zloc
    (or (and (branch? zloc) (down zloc))
        (right zloc)
        (recur zloc))))

(comment

  (def a-zip
    (zip [:a :b [:x]]))

  (node (df-next a-zip))
  # => :a

  (-> a-zip
      df-next
      df-next
      node)
  # => :b

  (-> a-zip
      df-next
      df-next
      df-next
      df-next
      df-next
      end?)
  # => true

  )

(defn replace
  "Replaces existing node at `zloc` with `node`, without moving."
  [zloc node]
  (let [[_ st] zloc]
    [node
     (make-state zloc
                 (st :ls)
                 (st :rs)
                 (st :pnodes)
                 (st :pstate)
                 true)]))

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
  ``
  Replaces the node at `zloc` with the value of `(f node args)`,
   where `node` is the node associated with `zloc`.
  ``
  [zloc f & args]
  (replace zloc
           (apply f (node zloc) args)))

(comment

  (-> (zip [1 2 [8 9]])
      down
      (edit inc)
      root)
  # => [2 2 [8 9]]

  )

(defn insert-child
  ``
  Inserts `child` as the leftmost child of the node at `zloc`,
  without moving.
  ``
  [zloc child]
  (replace zloc
           (make-node zloc
                      (node zloc)
                      [child ;(children zloc)])))

(comment

  (-> (zip [:a :b [:x :y]])
      (insert-child :c)
      root)
  # => [:c :a :b [:x :y]]

  )

(defn append-child
  ``
  Appends `child` as the rightmost child of the node at `zloc`,
  without moving.
  ``
  [zloc child]
  (replace zloc
           (make-node zloc
                      (node zloc)
                      [;(children zloc) child])))

(comment

  (-> (zip [:a :b [:x :y]])
      (append-child :c)
      root)
  # => [:a :b [:x :y] :c]

  )

(defn rightmost
  ``
  Returns the z-location of the rightmost sibling of the node at
  `zloc`, or the current node's z-location if there are none to the
  right.
  ``
  [zloc]
  (let [[node st] zloc
        {:ls ls :rs rs} st]
    (if (and (not (empty? st))
             (indexed? rs)
             (not (empty? rs)))
      [(last rs)
       (make-state zloc
                   (s/tuple-push ls node ;(s/butlast rs))
                   []
                   (st :pnodes)
                   (st :pstate)
                   (st :changed?))]
      zloc)))

(comment

  (-> (zip [:a :b [:x :y]])
      down
      rightmost
      node)
  # => [:x :y]

  )

(defn remove
  ``
  Removes the node at `zoc`, returning the z-location that would have
  preceded it in a depth-first walk.
  Throws an error if called at the root z-location.
  ``
  [zloc]
  (let [[node st] zloc
        {:ls ls
         :pnodes pnodes
         :pstate pstate
         :rs rs} st]
    #
    (defn recur
      [a-zloc]
      (if-let [child (and (branch? a-zloc) (down a-zloc))]
        (recur (rightmost child))
        a-zloc))
    #
    (if (not (empty? st))
      (if (pos? (length ls))
        (recur [(last ls)
                (make-state zloc
                            (s/butlast ls)
                            rs
                            pnodes
                            pstate
                            true)])
        [(make-node zloc (last pnodes) rs)
         (make-state zloc
                     (pstate :ls)
                     (pstate :rs)
                     (pstate :pnodes)
                     (pstate :pstate)
                     true)])
      (error "Called `remove` at root"))))

(comment

  (-> (zip [:a :b [:x :y]])
      down
      right
      remove
      node)
  # => :a

  (try
    (remove (zip [:a :b [:x :y]]))
    ([e] e))
  # => "Called `remove` at root"

  )

(defn left
  ``
  Returns the z-location of the left sibling of the node
  at `zloc`, or nil if there is no such sibling.
  ``
  [zloc]
  (let [[node st] zloc
        {:ls ls :rs rs} st]
    (when (and (not (empty? st))
               (indexed? ls)
               (not (empty? ls)))
      [(last ls)
       (make-state zloc
                   (s/butlast ls)
                   [node ;rs]
                   (st :pnodes)
                   (st :pstate)
                   (st :changed?))])))

(comment

  (-> (zip [:a :b :c])
      down
      right
      right
      left
      node)
  # => :b

  (-> (zip [:a])
      down
      left)
  # => nil

  )

(defn df-prev
  ``
  Moves to the previous z-location, depth-first.
  If already at the root, returns nil.
  ``
  [zloc]
  #
  (defn recur
    [a-zloc]
    (if-let [child (and (branch? a-zloc)
                        (down a-zloc))]
      (recur (rightmost child))
      a-zloc))
  #
  (if-let [left-loc (left zloc)]
    (recur left-loc)
    (up zloc)))

(comment

  (-> (zip [:a :b [:x :y]])
      down
      right
      df-prev
      node)
  # => :a

  (-> (zip [:a :b [:x :y]])
      down
      right
      right
      down
      df-prev
      node)
  # => [:x :y]

  )

(defn insert-right
  ``
  Inserts `a-node` as the right sibling of the node at `zloc`,
  without moving.
  ``
  [zloc a-node]
  (let [[node st] zloc
        {:ls ls :rs rs} st]
    (if (not (empty? st))
      [node
       (make-state zloc
                   ls
                   [a-node ;rs]
                   (st :pnodes)
                   (st :pstate)
                   true)]
      (error "Called `insert-right` at root"))))

(comment

  (def a-zip
    (zip [:a :b [:x :y]]))

  (-> a-zip
      down
      (insert-right :z)
      root)
  # => [:a :z :b [:x :y]]

  (try
    (insert-right a-zip :e)
    ([e] e))
  # => "Called `insert-right` at root"

  )

(defn insert-left
  ``
  Inserts `a-node` as the left sibling of the node at `zloc`,
  without moving.
  ``
  [zloc a-node]
  (let [[node st] zloc
        {:ls ls :rs rs} st]
    (if (not (empty? st))
      [node
       (make-state zloc
                   (s/tuple-push ls a-node)
                   rs
                   (st :pnodes)
                   (st :pstate)
                   true)]
      (error "Called `insert-left` at root"))))

(comment

  (def a-zip
    (zip [:a :b [:x :y]]))

  (-> a-zip
      down
      (insert-left :z)
      root)
  # => [:z :a :b [:x :y]]

  (try
    (insert-left a-zip :e)
    ([e] e))
  # => "Called `insert-left` at root"

  )

(defn rights
  "Returns siblings to the right of `zloc`."
  [zloc]
  (when-let [st (state zloc)]
    (st :rs)))

(comment

  (-> (zip [:a :b [:x :y]])
      down
      rights)
  # => [:b [:x :y]]

  )

(defn lefts
  "Returns siblings to the left of `zloc`."
  [zloc]
  (if-let [st (state zloc)
           ls (st :ls)]
    ls
    []))

(comment

  (-> (zip [:a :b])
      down
      lefts)
  # => []

  (-> (zip [:a :b [:x :y]])
      down
      right
      right
      lefts)
  # => [:a :b]

  )

(defn leftmost
  ``
  Returns the z-location of the leftmost sibling of the node at `zloc`,
  or the current node's z-location if there are no siblings to the left.
  ``
  [zloc]
  (let [[node st] zloc
        {:ls ls :rs rs} st]
    (if (and (not (empty? st))
             (indexed? ls)
             (not (empty? ls)))
      [(first ls)
       (make-state zloc
                   []
                   [;(s/rest ls) node ;rs]
                   (st :pnodes)
                   (st :pstate)
                   (st :changed?))]
      zloc)))

(comment

  (-> (zip [:a :b [:x :y]])
      down
      leftmost
      node)
  # => :a

  (-> (zip [:a :b [:x :y]])
      down
      rightmost
      leftmost
      node)
  # => :a

  )

(defn path
  "Returns the path of nodes that lead to `zloc` from the root node."
  [zloc]
  (when-let [st (state zloc)]
    (st :pnodes)))

(comment

  (path (zip [:a :b [:x :y]]))
  # => nil

  (-> (zip [:a :b [:x :y]])
      down
      path)
  # => [[:a :b [:x :y]]]

  (-> (zip [:a :b [:x :y]])
      down
      right
      right
      down
      path)
  # => [[:a :b [:x :y]] [:x :y]]

 )

(defn right-until
  ``
  Try to move right from `zloc`, calling `pred` for each
  right sibling.  If the `pred` call has a truthy result,
  return the corresponding right sibling.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when-let [right-sib (right zloc)]
    (if (pred right-sib)
      right-sib
      (right-until right-sib pred))))

(comment

  (-> [:code
       [:tuple
        [:comment "# hi there"] [:whitespace "\n"]
        [:symbol "+"] [:whitespace " "]
        [:number "1"] [:whitespace " "]
        [:number "2"]]]
      zip
      down
      right
      down
      (right-until |(match (node $)
                      [:comment]
                      false
                      #
                      [:whitespace]
                      false
                      #
                      true))
      node)
  # => [:symbol "+"]

  )

(defn search-from
  ``
  Successively call `pred` on z-locations starting at `zloc`
  in depth-first order.  If a call to `pred` returns a
  truthy value, return the corresponding z-location.
  Otherwise, return nil.
  ``
  [zloc pred]
  (if (pred zloc)
    zloc
    (when-let [next-zloc (df-next zloc)]
      (when (end? next-zloc)
        (break nil))
      (search-from next-zloc pred))))

(comment

  (-> (zip [:a :b :c])
      down
      (search-from |(match (node $)
                      :b
                      true))
      node)
  # => :b

  (-> (zip [:a :b :c])
      down
      (search-from |(match (node $)
                      :d
                      true)))
  # => nil

  (-> (zip [:a :b :c])
      down
      (search-from |(match (node $)
                      :a
                      true))
      node)
  # => :a

  )

(defn search-after
  ``
  Successively call `pred` on z-locations starting after
  `zloc` in depth-first order.  If a call to `pred` returns a
  truthy value, return the corresponding z-location.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when (end? zloc)
    (break nil))
  (when-let [next-zloc (df-next zloc)]
    (if (pred next-zloc)
      next-zloc
      (search-after next-zloc pred))))

(comment

  (-> (zip [:b :a :b])
      down
      (search-after |(match (node $)
                       :b
                       true))
      left
      node)
  # => :a

  (-> (zip [:b :a :b])
      down
      (search-after |(match (node $)
                       :d
                       true)))
  # => nil

  (-> (zip [:a [:b :c [2 [3 :smile] 5]]])
      (search-after |(match (node $)
                       [_ :smile]
                       true))
      down
      node)
  # => 3

  )

(defn unwrap
  ``
  If the node at `zloc` is a branch node, "unwrap" its children in
  place.  If `zloc`'s node is not a branch node, do nothing.
  ``
  [zloc]
  (unless (branch? zloc)
    (break zloc))
  #
  (def kids (children zloc))
  (var i 0)
  (var kid (get kids 0))
  (var curr-zloc zloc)
  (while (and kid
              (< i (length kids)))
    (set curr-zloc
         (insert-left curr-zloc kid))
    (++ i)
    (set kid (get kids i)))
  (remove curr-zloc))

(comment

  (-> (zip [:a :b [:x :y]])
      down
      right
      right
      unwrap
      root)
  # => [:a :b :x :y]

  (-> (zip [:a :b [:x :y]])
      down
      unwrap
      root)
  # => [:a :b [:x :y]]

  )
