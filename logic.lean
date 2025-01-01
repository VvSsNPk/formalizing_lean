example (α : Type) (p q : α → Prop) : (∀ x : α, p x ∧ q x) → ∀ y : α, p y :=
  fun h : ∀ x : α, p x ∧ q x =>
    fun y : α =>
      show p y from (h y).left


#check Eq.refl

example : 2 + 3 = 5 := by rfl

example (α : Type) (a b : α) (p : α → Prop)
  (h1 : a = b) (h2 : p a) : p b :=
  h1 ▸ h2

variable (a b c d e : Nat)
variable (h1 : a = b)
variable (h2 : b = c + 1)
variable (h3 : c = d)
variable (h4 : e = 1 + d)

