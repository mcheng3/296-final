(ns adventure.core)
(require '[clojure.string :as str])

;; # Interaction
;; A simple parser that can remember facts.
;;

;; # Actions
;;
;; Let an *action* be a function with a specific
;; interface: It will take in a *state* followed by
;; a sequence of arguments.  It will return a vector
;; consisting of a new state and a response string.
;;
;; Here is a simple example, a post-increment function.
;; Assume the state is a hash-map with a key :vars
;; and a value containing a hash of the variables and
;; their values.


(defn post-increment
  "Action: post-increment
   Returns the value of a variable from the state and increments it."
  [state var]
  (if-let [val (get-in state [:vars var])]
    [(update-in state [:vars var] inc) val]
    [(assoc-in state [:vars var] 1) 0]))

;; <pre><code>
;; interaction.core=> (post-increment {:vars {:x 10}} :x)
;; [{:vars {:x 11}} 10]
;; </code></pre>

;; ## Your work
;;
;; Fill in the code for these functions.
;;

(defn lookup-var
  "Given a state and a variable name, return the value of the variable
  if it has been defined, otherwise return 0."
  [state var]
  (if-let [val (get-in state [:vars var])]
	[state (get-in state [:vars var])]
	[state 0]))

;; <pre><code>
;; interaction.core=> (lookup-var {:vars {:x 10}} :x)
;; [{:vars {:x 10}} 10]
;; </code></pre>

(defn set-plus-var
  "Given a state and a variable name, return the value of the variable
  if it has been defined, otherwise return 0."
  [state var]
  (if-let [val (get-in state [:vars var])]
	(get-in state [:vars var])
	var))


(defn set-plus
  "Action: set-plus.  Set var = e1 + e2, return the sum as a result."
  [state var e1 e2]
    [(assoc-in state [:vars var] (+ (set-plus-var state e1) (set-plus-var state e2)))
	 	(+ (set-plus-var state e1) (set-plus-var state e2))]
  )

;; <pre><code>
;; interaction.core=> (set-plus {:vars {:x 10}} :y :x 20)
;; [{:vars {:x 10 :y 30}} 30]
;; </code></pre>

(defn set-var
  "Action: set-var. Set var = e1.  Return the new value as a result."
  [state var e1]
  [(assoc-in state [:vars var] (set-plus-var state e1)) (set-plus-var state e1)])


;; <pre><code>
;; interaction.core=> (set-var {:vars {:x 10}} :y :x)
;; [{:vars {:x 10 :y 10}} 10]
;; </code></pre>

(defn there-is-a
  "Action: there-is-a.  Remember that an object obj exists.
  Returns \"There is a obj\" as a result."
  [state object]
  (if-let [val (get-in state [:objects object])]
	  [state (str "There is a " (name object) ".")]
	  [(assoc-in state [:objects object] []) (str "There is a " (name object) ".")]
  )
  )

(defn go [state dir]
  (let [location (get-in state [:adventurer :location])
        dest ((get-in state [:map location :dir]) dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          state)
      (update-in (assoc-in state [:adventurer :location] dest) [:adventurer :tick] inc))))

(defn list-inventory 
	[state]
	(let [inv (get-in state [:adventurer :inventory])]
		(do (println (str "You have:")) (apply println  (conj (vec(map #(str % ",")(map name (butlast inv)))) 
		(name (last inv)))) state
	)))

(defn take-item
	[state]
	(do (println "Inventory") state)
	)

(defn drop-item
	[state]
	(do (println "Inventory") state)
	)

(defn single [state word]
	(cond
		(or (= word :north) (= word :n)) (go state :north) 
		(or (= word :south) (= word :s)) (go state :south) 
		(or (= word :west) (= word :w)) (go state :west) 
		(or (= word :east) (= word :e)) (go state :east) 
		(or (= word :inventory) (= word :i)) (list-inventory state)
		(= word :quit) (System/exit 0)
		:else (do (println "I don't know what you mean. ") state)
		))

;; <pre><code>
;; interaction.core=> (there-is-a {:vars {:x 10}} :shoe)
;; [{:vars {:x 10 :y 10}
;;   :objects {:shoe []}} "There is a shoe."]
;; </code></pre>

(defn the-obj-is
  "Action: there-obj-a.  Remember an adjective that applies to an object.
  Returns \"The obj is adj\" as a result."
  [state object adj]
  (if (some #{adj} (get-in state [:objects object]))
	  [state (str "The " (name object) " is " (name adj) ".")]
	  [(assoc-in state [:objects object] (conj (get-in state [:objects object]) adj)) (str "The " (name object) " is " (name adj) ".")]
  )
  )
;; <pre><code>
;; interaction.core=> (the-obj-is {:vars {:x 10} :objects {:shoe []}} :shoe :blue)
;; [{:vars {:x 10} :objects {:shoe [:blue]}} "The shoe is blue."]
;; </code></pre>




(defn describe-obj
  "Describe the given object \"The obj is adj\" if it exists in state . If not, return \"There is no obj\""
  [state object]
  (do (if-let [val (get-in state [:adventurer :inventory object])]
	  (println (get-in state [:items object :desc]))
	  (println (str "There is no " (name object) "."))
  ) state)
)
;; <pre><code>
;; interaction.core=> (describe-obj  {:vars {:x 10} :objects {:shoe [:blue :big]}} :shoe)
;; [{:vars {:x 10}, :objects {:shoe [:blue :big]}} "The shoe is blue." "The shoe is big."]
;; </code></pre>


(defn forget-obj
  "Delete the given object and return \"What obj?\""
  [state object]
	[(update-in state [:objects] dissoc object) (str "What " (name object) "?")]
  )
;; <pre><code>
;; interaction.core=> (forget-obj {:objects {:show [:exciting]}} :show)
;; [{:objects {}} "What show?"]
;; </code></pre>


;; # Action Environment
;;
;; The runtime environment is a vector of the form
;;
;; ``` [ phrase action phrase action ...]```
;;
;; The "phrase" is a canonicalized vector of keywords.
;; (Remember that a keyword is a symbol starting with a colon, like :name.)
;; The `@` character will represent a variable.


(def initial-env [  [:postinc "@"] post-increment ["@"] single 
		[:go "@"] go [:describe "@"] describe-obj [:look "@"] describe-obj 
		[:examine "@"] describe-obj [:take "@"] take-item [:drop "@"] drop-item
			])  ;; add your other functions here

;; # Parsing
;;
;; This is mostly code from the lecture.

(defn canonicalize
  "Given an input string, strip out whitespaces, lowercase all words, and convert to a vector of keywords."
  [input]
  (vec (map keyword (map str/lower-case (vec (str/split input #"( |\.)")))))
  )

;; <pre><code>
;; interaction.core> (canonicalize "The shoe is blue.")
;; [:the :shoe :is :blue]
;; </code></pre>
(defn match [pattern input]
	(loop [pattern pattern
			input input
			vars `()]
			(cond (and (empty? pattern) (empty? input)) (reverse vars)
			(or (empty? pattern) (empty? input)) nil
			(= (first pattern) "@")
				(recur (rest pattern)
					(rest input)
					(cons (first input) vars))
			(= (first pattern) (first input))
				(recur (rest pattern)
						(rest input)
						vars)
			:fine-be-that-way nil)))

;; Credits to Sam Ritchie
(defn first-non-nil
  [xs]
  (if (empty? (first (filter (complement nil?)
                 xs)))
	nil
	(+ (* (.indexOf xs (first (filter (complement nil?)
                 xs)) )2)1)))
(defn react
  "Given a state and a canonicalized input vector, search for a matching phrase and call its corresponding action.
  If there is no match, return the original state and result \"I don't know what you mean.\""
  [state input-vector]
	(if (first-non-nil (map #(match % input-vector) (take-nth 2 initial-env)))
		(apply (nth initial-env (first-non-nil (map #(match % input-vector) (take-nth 2 initial-env))))
		(conj (match (nth initial-env (- (first-non-nil (map #(match % input-vector) (take-nth 2 initial-env))) 1))  input-vector) state))
		(do (println "I don't know what you mean. ") state)))


;; <pre><code>
;; interaction.core> (react {:vars {:x 10} :runtime initial-env} [:postinc :x])
;; [ {:vars {:x 11} :runtime { ... omitted for space ... }}  10]
;; </code></pre>

(defn repl
  "Start a REPL using the given environment.  The :runtime key should map to the action environment.
  Prints out the result and loops the state for another round.  Quits when you say bye.
  You may need (flush) to print out '>' without a newline "
  [env]
  )

;; <pre><code>
;; interaction.core=> (repl initial-env)
;; Welcome!  Let's talk.
;; > there is a spoon.
;; There is a spoon.
;; > the spoon is blue.
;; The spoon is blue.
;; > the spoon is big.
;; The spoon is big.
;; > describe the spoon.
;; The spoon is blue.
;; The spoon is big.
;; > forget the spoon.
;; What spoon?
;; > describe the spoon.
;; There is no spoon.
;; > bye
;; nil
;; interaction.core=>
;; </code></pre>

(def init-map
  {:foyer {:desc "The walls are freshly painted but do not have any pictures.  You get the feeling it was just created for a game or something."
           :title "in the foyer"
           :dir {:south :grue-pen}
           :contents #{:raw-egg}}
   :grue-pen {:desc "It is very dark.  You are about to be eaten by a grue."
              :title "in the grue pen"
              :dir {:north :foyer}
              :contents #{}}
   })

(def init-adventurer
  {:location :foyer
   :inventory #{:usb}
   :hp 10
   :lives 3
   :tick 0
   :seen #{}})

   (def init-items
 {:raw-egg {:desc "This is a raw egg.  You probably want to cook it before eating it."
            :name "a raw egg" }
   :usb {:desc "This is a USB drive. Use to download Windows 10."
            :name "a USB drive" }})

(defn main
  "Start the REPL with the initial environment."
  []
  (repl initial-env)

  )



(defn status [state]
  (let [location (get-in state [:adventurer :location])
        the-map (:map state)]
    (println location)
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((get-in state [:adventurer :seen]) location)
      (print (-> the-map location :desc)))
    (update-in state [:adventurer :seen] #(conj % location))))

(defn -main
  "Initialize the adventure"
  [& args]
  (loop [local-state {:map init-map :adventurer init-adventurer :items init-items}]
    (let [pl (status local-state) 
          _  (println " What do you want to do?")
          command (read-line)]

      (recur (react pl (canonicalize command)) ))))