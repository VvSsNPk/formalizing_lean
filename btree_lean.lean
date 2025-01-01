inductive  BTree Int where
  | leaf : BTree Int
  | node  (β : Int) (left : BTree Int) (right : BTree Int) : BTree Int
  deriving Inhabited


def BTree.print ( α : BTree Int) : String :=
  match α with
  | leaf => "leaf"
  | node x y z => s!"({x},{print y},{print z})"

instance : ToString (BTree Int) where
  toString := BTree.print


def x : BTree Int := BTree.leaf

def y : BTree Int := BTree.node (2) BTree.leaf BTree.leaf

def BTree.Insert : BTree Int -> Int -> BTree Int
  | leaf,γ => node γ leaf leaf
  | node δ x y, γ => if (γ <= δ) then
    node δ (Insert x γ) y 
    else
      node δ x (Insert y γ)

def z : BTree Int := y.Insert 5

def inorder_trevarsel (α : BTree Int): List Int:=
  match α with
  | BTree.leaf => []
  | BTree.node x y z =>  inorder_trevarsel y ++ [x]++ inorder_trevarsel z

def m : BTree Int := ((z.Insert 6).Insert 8).Insert 0



