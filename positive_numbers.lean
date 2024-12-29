inductive Pos : Type where 
| one : Pos 
| succ : Pos -> Pos

def Pos_to_Nat (p : Pos) : Nat :=
  match p with 
  | Pos.one => 1
  | .succ n => 1 + Pos_to_Nat n

class Plus (α : Type) where
  plus : α → α → α

instance : Plus Nat where
  plus := Nat.add

def add_pos (α : Pos) (β : Pos) : Pos :=
  match α with
  | .one => .succ β
  | .succ γ => .succ (add_pos γ β)

def seven : Pos := Pos.succ (Pos.succ (Pos.succ (Pos.succ (Pos.succ (Pos.succ (Pos.one))))))

instance : ToString Pos where
  toString x := toString (Pos_to_Nat x)

#eval (toString seven)
#eval toString (add_pos seven seven)


def mul_pos (a : Pos) (b: Pos) : Pos :=
  match a with
  | .one => b
  | .succ n => add_pos b (mul_pos n b)

instance : Add Pos where
  add := add_pos

instance : Mul Pos where
  mul := mul_pos
#eval toString (seven * seven)
#eval toString (seven + seven)

structure Pos2 where
  succ :: (pred : Nat)


def add_pos2 (a : Pos2) (b : Pos2) : Pos2 := 
  match a with
  | .succ n => match b with
              | .succ m => .succ (n + m + 1)

instance : Add Pos2 where
  add := add_pos2

#eval  Pos2.succ 7 + Pos2.succ 5
#check (IO.println seven)
#eval ([] : List Float).sum

instance : OfNat Pos2 (n + 1) where
  ofNat := match n  with
          | 0 => Pos2.succ 0
          | Nat.succ x => Pos2.succ (1+x)

class HPlus (α : Type) (β : Type) (γ : outParam Type) where
  hplus : α → β → γ

def addNatPos : Nat → Pos → Pos
| 0,p => p 
|n+1,p => Pos.succ (addNatPos n p)

def addNatPos2 : Nat -> Pos -> Nat
| 0,p => Pos_to_Nat p
| Nat.succ x, p => Nat.succ (addNatPos2 x p)

instance : HPlus Nat Pos Pos where
  hplus := addNatPos

@[default_instance]
instance : HPlus Nat Pos Nat where
  hplus := addNatPos2


def x : Nat := (HPlus.hplus 3 (Pos.one))
#eval x


