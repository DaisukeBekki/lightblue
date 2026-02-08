# README

## **What is `wani`?**

- *wani* is a prover with DTS representations
    - $\Pi$ type
    - $\Sigma$ type
    - Equal type (partial implementation)
    - Disjoint Union type (partial implementation)

## How to use Wani

There are two ways to use wani.

1. Using natural language (Japanese) as an input
2. Using DTT as an input

### Using natural language as an input
If you want to use natural language, you can prepare [JSeM](https://github.com/DaisukeBekki/JSeM) file and use *lightblue* as a parser.

```
stack run lightblue <lang> <lang's local options> jsem <command's local options> <global options>
```

**Quick Start**

1. Prepare `hello.xml`.
```
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE jsem-dataset SYSTEM "jsem.dtd">
<?xml-stylesheet type="text/xsl" href="jsem.xsl"?>
<jsem-dataset>
  <problem answer="yes" inference_type="entailment" jsem_id="hello-jp-1" language="jp">
    <p idx="1">
      <script>ワニは緑色の爬虫類だ。</script>
    </p>
    <h>
      <script>ワニは爬虫類だ。</script>
    </h>
    <note>p : Wanis are green reptiles. h : Wanis are reptiles.</note>
  </problem>
  <problem answer="unknown" inference_type="entailment" jsem_id="hello-jp-2" language="jp">
    <p idx="1">
      <script>ワニは緑色の爬虫類だ。</script>
    </p>
    <h>
      <script>ワニは哺乳類だ。</script>
    </h>
    <note>p : Wanis are green reptiles. h : Wanis are mammals. (FYI : In this problem, `Wani` does not know that reptiles cannot be a mammals)</note>
  </problem>
</jsem-dataset>
```

2. To prove prepared JSeM problems, execute:
```
stack run lightblue -- jp -m juman jsem --jsemid hello-jp-1 --nsample 1 -s html -f <file path of `hello.xml`> --noTypeCheck --nproof 1 --nparse 1 --ntypecheck 1 --maxdepth 5 --maxtime 1000

stack run lightblue -- jp -m juman jsem --jsemid hello-jp-2 --nsample 1 -s html -f <file path of `hello.xml`> --noTypeCheck --nproof 1 --nparse 1 --ntypecheck 1 --maxdepth 5 --maxtime 1000
```

### Using DTT as an input

You can use DTT-style input.

**Quick Start**

1. Prepare `hello.xml`.

ref : [sample-repositry](https://github.com/hinarid/wani-example), [test-repository](https://github.com/hinarid/waniTest/blob/main/README.md, )

# 🧩 Syntax Correspondence Table

This table shows how each Haskell constructor in the `Preterm` datatype
corresponds to logical / type-theoretic notation.

| Haskell Expression | Logical / Type-Theoretic Form | Intuitive Meaning                       |
| ------------------ | ----------------------------- | --------------------------------------- |
| `Var 0`          | `N/A`                          | Variable (de Bruijn index 0)            |
| `Con "p"`        | `p`                           | Constant symbol `p`                     |
| `Type`           | `type`                        | The sort of all small types             |
| `Kind`           | `kind`                        | The sort of all kinds                   |
| `Pi A B`         | `Π (x : A). B`                | Dependent function type (generalized ∀) |
| `Lam t`          | `λx. t`                       | Lambda abstraction                      |
| `App f x`        | `f x`                         | Function application                    |
| `Not A`          | `¬A`                          | Negation                                |
| `Sigma A B`      | `Σ (x : A). B`                | Dependent pair type (existential type)  |
| `Pair a b`       | `(a, b)`                      | Pair term                               |
| `Proj Fst p`   | `\pi_1 p`                       | First projection                        |
| `Proj Snd p`   | `\pi_2 p`                       | Second projection                       |
| `Disj A B`       | `A + B`                       | Disjoint union (sum) type               |
| `Iota Fst t`   | `inl t`                       | Left injection                          |
| `Iota Snd t`   | `inr t`                       | Right injection                         |
| `Unpack p l m n` | `unpack p as m,n in l`        | Eliminator for disjoint union type                 |
| `Bot`            | `⊥`                           | Bottom (falsehood)                      |
| `Top`            | `⊤`                           | Top (truth)                             |
| `Entity`         | `Entity`                      | Type of entities (individuals)          |
| `Eq A x y`       | `x =_{[A]} y`                   | Intensional equality                    |

---

## 🌿 De Bruijn indices and binding

wani’s `Preterm` uses **De Bruijn indices** to represent variables.
An integer `Var n` refers to “the variable bound by the `n`-th nearest binder”.

### 🧩 Key rule

* When you **go under a binder** (like `Lam`, `Pi`, or `Sigma`),
  all *free variable indices* increase by `+1`.
* When you **apply** or **pair** terms, there’s **no new binder**, so indices stay the same.

---

### 📘 Examples

| Concrete syntax          | `Preterm` (De Bruijn)                                                    | Notes                                                                                                  |
| ------------------------ | ------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------ |
| `λx. x`                  | `Lam (Var 0)`                                                            | `Var 0` refers to `x` (innermost binder).                                                              |
| `λx. λy. x`              | `Lam (Lam (Var 1))`                                                      | `Var 1` refers to the *outer* `x`.                                                                     |
| `λx. f x`                | `Lam (App (Con "f") (Var 0))`                                            | `App` does *not* shift indices.                                                                        |
| `λx. y` with context `y`      | `(Lam (Var 1)`                                                      | `Var 1` refers to the *outside* of `Lam`.                                                                     |
| `λx. Σy:A. Pair x y`     | `Lam (Sigma (Con "A") (Pair (Var 1) (Var 0)))`                           | In `Pair`, both `Var 1` (`x`) and `Var 0` (`y`) appear. No extra shift because `Pair` is not a binder. |
| `Σx:A. Σy:B x. Pair x y` | `Sigma (Con "A") (Sigma (App (Con "B") (Var 0)) (Pair (Var 1) (Var 0)))` | Inner `Var 1` refers to outer `x`.                                                                     |
| `λx. π₁ (Pair x y)`      | `Lam (Proj Fst (Pair (Var 0) (Con "y")))`                                | `Proj` just selects; no binding, no index shift.                                                       |

---

### 🧮 Summary

| Constructor                                                      | Introduces binding? | Effect on indices           |
| ---------------------------------------------------------------- | ------------------- | --------------------------- |
| `Lam`, `Pi`, `Sigma`               | ✅ yes               | increments free vars inside |
| others | ❌ no                | no change                   |

---

### 🧠 Example

Here’s how a relatively complex term like this:

```haskell
U.Pi
  (U.Sigma (U.Entity) (U.App (U.Con "aProf") (U.Var 0)))
  (U.App (U.Con "human") (U.Proj U.Fst (U.Var 0)))
```

corresponds to a dependent type:

```
Π (x : Σ (e : Entity). aProf(e)). human(\pi_1(x))
```

In natural language:

> “Every assistant professor is a human.”

---

## 📚 References

> 🧩 This repository accompanies and extends the ideas developed in:
>
- **Hinari Daido** (2022)
*DTS の部分体系を用いた
定理自動証明器への
等号型の導入*  
  Master’s Thesis, The Ochanomizu University.

Related foundational works include:

- **Hinari Daido and Daisuke Bekki.** (2017). *Development of an automated theorem prover for the fragment of DTS.*
  In the 17th International Workshop on Logic and Engineering of Natural Language Semantics (LENLS17).

- **Asa Tomita and Mai Matsubara and Hinari Daido and Daisuke Bekki.** (2025). *Natural Language Inference with CCG Parser
and Automated Theorem Prover for DTS*  
  Proceedings of Bridges and Gaps between Formal and Computational Linguistics (BriGap-2), Workshop in IWCS

These works provide the theoretical and type-theoretic background for the proof search system demonstrated here.