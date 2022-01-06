type Tuple<X, Y> = [X, Y];

type Left<T extends Tuple<any, any>> = T extends Tuple<infer L, any>
  ? L
  : never;
type Right<T extends Tuple<any, any>> = T extends Tuple<any, infer R>
  ? R
  : never;

// EQ_<Expected, Given>
//
// EQ_ returns a tuple where the first element is the Given type and the second
// element is the truthiness of the equality. It's meant to be used with Assert
// so that Assert can check the truthiness and return the type for further use
// or inspection.
type EQ_<N, M> = N extends M
  ? M extends N
    ? Tuple<M, true>
    : Tuple<M, false>
  : Tuple<M, false>;

// Use this only when truthiness counts. Name and signature compatible with
// other relationship like LT, LTE, GT, GTE
type EQ<N, M> = Right<EQ_<N, M>>;

// Simple way to static assert a type expression that accepts only a true type
// (or a tuple returned from EQ_) otherwise you get a compile error. I searched
// for quite some time for a way to check assumptions on types and the solution
// is pretty simple: `type Assert<X extends true> = X`
type Assert<X extends true | Tuple<any, true>> = X extends true
  ? true
  : X extends Tuple<infer Y, true>
  ? Y
  : never;

type Refute<X extends false | Tuple<any, false>> = X extends false
  ? false
  : X extends Tuple<infer Y, false>
  ? Y
  : never;

type Not<X extends Tuple<any, boolean>> = X extends Tuple<infer Y, true>
  ? Tuple<Y, false>
  : X extends Tuple<infer Y, false>
  ? Tuple<Y, true>
  : never;

type IsNever<X extends any> = X extends never ? true : false;

// Convenient way to represent natural numbers, possible only because we have
// "Template Literal Types", aka we have a way to manipulate name of types as
// they were strings.
//
// It's convenient because some operations can be implemented efficiently with
// string concatenation (ex. Add) or splitting (ex. Sub, LT, ...)
type Natural = string;
type Zero = "";
type One = `.${Zero}`;
type Two = `.${One}`;
type Three = `.${Two}`;
type Four = `.${Three}`;
type Five = `.${Four}`;
type Six = `.${Five}`;
type Seven = `.${Six}`;
type Eight = `.${Seven}`;
type Nine = `.${Eight}`;
type Ten = `.${Nine}`;

type _00100 = Assert<EQ_<One, One>>;
type _00101 = Refute<EQ_<Two, One>>;
type _00102 = Assert<EQ_<Zero, Zero>>;
type _00103 = Refute<Not<EQ_<Zero, Zero>>>;
type _00104 = Refute<EQ_<One, never>>;
// @ts-expect-error cannot compare with any
type _00105 = Refute<EQ_<One, any>>;
// @ts-expect-error cannot compare with any
type _00106 = Refute<EQ_<any, One>>;
type _00107 = Refute<EQ_<unknown, One>>;
type _00108 = Refute<EQ_<One, unknown>>;

type LT<N extends Natural, M extends Natural> = N extends `${infer _}${M}`
  ? false
  : true;

type LTE<N extends Natural, M extends Natural> = N extends `${infer L}${M}`
  ? L extends ""
    ? true
    : false
  : true;

type GT<N extends Natural, M extends Natural> = N extends `${infer L}${M}`
  ? L extends ""
    ? false
    : true
  : false;

type GTE<N extends Natural, M extends Natural> = N extends `${infer _}${M}`
  ? true
  : false;

type _00200 = Assert<LT<One, Three>>;
type _00201 = Refute<LT<Three, Three>>;
type _00202 = Refute<LT<Four, Three>>;

type _00203 = Assert<LTE<One, Three>>;
type _00204 = Assert<LTE<Three, Three>>;
type _00205 = Refute<LTE<Four, Three>>;

type _00206 = Refute<GT<One, Three>>;
type _00207 = Refute<GT<Three, Three>>;
type _00208 = Assert<GT<Four, Three>>;

type _00209 = Refute<GTE<One, Three>>;
type _00210 = Assert<GTE<Three, Three>>;
type _00211 = Assert<GTE<Four, Three>>;

type Increment<X extends Natural> = `.${X}`;
type Decrement<X extends Natural> = EQ<X, Zero> extends true
  ? Zero
  : X extends `.${infer Y}`
  ? Y
  : never;

// Many of the implementations are tail call optimizable, meaning that the
// recursive invocation is the last/only expression. Most of the time it entails
// the use of an accumulator, in this case `Q`.
type DivMod<
  N extends Natural,
  M extends Natural,
  Q extends Natural = Zero
> = N extends `${infer X}${M}` ? DivMod<X, M, Increment<Q>> : Tuple<Q, N>;

type Div<N extends Natural, M extends Natural> = Left<DivMod<N, M>>;
type Mod<N extends Natural, M extends Natural> = Right<DivMod<N, M>>;

type Add<N extends Natural, M extends Natural> = `${N}${M}`;
type Sub<N extends Natural, M extends Natural> = N extends `${infer R}${M}`
  ? R
  : never;
type Mul<
  N extends Natural,
  M extends Natural,
  A extends Natural = Zero
> = M extends Zero ? A : Mul<N, Decrement<M>, Add<N, A>>;

type _00300 = Assert<EQ_<One, Increment<Zero>>>;
type _00301 = Assert<EQ_<Two, Increment<One>>>;
type _00302 = Assert<EQ_<One, Decrement<Increment<One>>>>;

type _00303 = Assert<EQ_<Tuple<Two, One>, DivMod<Five, Two>>>;
type _00304 = Assert<EQ_<Tuple<One, Three>, DivMod<Seven, Four>>>;
type _00305 = Assert<EQ_<Tuple<Four, Zero>, DivMod<Eight, Two>>>;

type _00306 = Assert<EQ_<One, Add<One, Zero>>>;
type _00307 = Assert<EQ_<Add<One, Zero>, Add<Zero, One>>>;
type _00308 = Assert<EQ_<Three, Add<Two, One>>>;
type _00309 = Assert<EQ_<Add<Two, One>, Add<One, Two>>>;

type _00310 = Assert<EQ_<One, Sub<Three, Two>>>;
type _00311 = Assert<IsNever<Sub<Three, Four>>>;

type _00312 = Assert<EQ_<Six, Mul<Three, Two>>>;
type _00313 = Assert<EQ_<Mul<Two, Three>, Mul<Three, Two>>>;

type Sequence<N extends Natural, A extends Array<Natural> = []> = EQ<
  N,
  Zero
> extends true
  ? A
  : Sequence<Decrement<N>, [N, ...A]>;

type Count<N extends Array<any>, C extends Natural = Zero> = N extends []
  ? C
  : N extends [infer _, ...infer T]
  ? Count<T, Increment<C>>
  : never;

type _00400 = Assert<EQ_<Two, Count<[1, 2]>>>;
type _00401 = Assert<EQ_<Two, Count<Sequence<Two>>>>;
type _00402 = Assert<EQ_<Zero, Count<Sequence<Zero>>>>;
type _00403 = Assert<EQ_<[One, Two, Three], Sequence<Three>>>;

type ConcatS<L extends string, R extends string> = `${L}${R}`;
type ConcatA<L extends Array<any>, R extends Array<any>> = [...L, ...R];
type Concat<L, R> = L extends string
  ? R extends string
    ? ConcatS<L, R>
    : never
  : L extends Array<any>
  ? R extends Array<any>
    ? ConcatA<L, R>
    : never
  : never;

type FirstS<S extends string> = S extends `${infer H}${infer _}` ? H : never;
type FirstA<L extends Array<any>> = L extends [infer H, ...infer _] ? H : never;
type First<L> = L extends string
  ? FirstS<L>
  : L extends Array<any>
  ? FirstA<L>
  : never;

type Head<L> = First<L>;

type TailS<S extends string> = S extends `${infer _}${infer T}` ? T : never;
type TailA<L extends Array<any>> = L extends [infer _, ...infer T] ? T : never;
type Tail<L> = L extends string
  ? TailS<L>
  : L extends Array<any>
  ? TailA<L>
  : never;

type ReverseS<
  S extends string,
  A extends string = ""
> = S extends `${infer H}${infer T}` ? ReverseS<T, ConcatS<H, A>> : A;
type ReverseA<L extends Array<any>, A extends Array<any> = []> = L extends []
  ? A
  : L extends [infer H, ...infer T]
  ? ReverseA<T, [H, ...A]>
  : never;
type Reverse<L> = L extends string
  ? ReverseS<L>
  : L extends Array<any>
  ? ReverseA<L>
  : never;

type LastS<S extends string> = S extends "" ? never : FirstS<ReverseS<S>>;
type LastA<L extends Array<any>> = FirstA<ReverseA<L>>;
type Last<L> = L extends string
  ? LastS<L>
  : L extends Array<any>
  ? LastA<L>
  : never;

type NthA<N extends Natural, X extends Array<any>> = X extends [
  infer H,
  ...infer T
]
  ? N extends Zero
    ? H
    : NthA<Decrement<N>, T>
  : never;
type NthS<
  N extends Natural,
  X extends string
> = X extends `${infer H}${infer T}`
  ? N extends Zero
    ? H
    : NthS<Decrement<N>, T>
  : never;
type Nth<N extends Natural, L> = L extends string
  ? NthS<N, L>
  : L extends Array<any>
  ? NthA<N, L>
  : never;

type _00500 = Assert<EQ_<"123", ConcatS<"12", "3">>>;
type _00501 = Assert<EQ_<"", ConcatS<"", "">>>;
type _00502 = Assert<EQ_<[1, 2, 3], ConcatA<[1, 2], [3]>>>;
type _00503 = Assert<EQ_<[1, 2, 3], Concat<[1, 2], [3]>>>;
type _00504 = Assert<EQ_<"123", Concat<"12", "3">>>;
type _00505 = Assert<EQ_<[3, 2, 1], ReverseA<[1, 2, 3]>>>;
type _00506 = Assert<EQ_<3, LastA<[1, 2, 3]>>>;
type _00507 = Assert<EQ_<"1", FirstS<"123">>>;
type _00508 = Assert<EQ_<"3", LastS<"123">>>;
type _00509 = Assert<EQ_<"23", TailS<"123">>>;
type _00510 = Assert<EQ_<1, NthA<Zero, [1, 2, 3]>>>;
type _00511 = Assert<EQ_<2, NthA<One, [1, 2, 3]>>>;
type _00512 = Assert<EQ_<3, NthA<Two, [1, 2, 3]>>>;
type _00513 = Assert<EQ_<"1", NthS<Zero, "123">>>;
type _00514 = Assert<EQ_<"2", NthS<One, "123">>>;
type _00515 = Assert<EQ_<"3", NthS<Two, "123">>>;
type _00516 = Assert<EQ_<3, Nth<Two, [1, 2, 3]>>>;
type _00517 = Assert<EQ_<"3", Nth<Two, "123">>>;

type ToString<N extends Natural, S extends string = ""> = N extends Zero
  ? S extends ""
    ? "0" // Original input is Zero
    : S // Recursion base case
  : DivMod<N, Ten> extends Tuple<infer Q, infer R>
  ? Q extends Natural
    ? R extends Natural
      ? R extends Zero
        ? ToString<Q, ConcatS<"0", S>>
        : R extends One
        ? ToString<Q, ConcatS<"1", S>>
        : R extends Two
        ? ToString<Q, ConcatS<"2", S>>
        : R extends Three
        ? ToString<Q, ConcatS<"3", S>>
        : R extends Four
        ? ToString<Q, ConcatS<"4", S>>
        : R extends Five
        ? ToString<Q, ConcatS<"5", S>>
        : R extends Six
        ? ToString<Q, ConcatS<"6", S>>
        : R extends Seven
        ? ToString<Q, ConcatS<"7", S>>
        : R extends Eight
        ? ToString<Q, ConcatS<"8", S>>
        : R extends Nine
        ? ToString<Q, ConcatS<"9", S>>
        : never
      : never
    : never
  : never;

type FromString<
  N extends String,
  M extends Natural = One,
  R extends Natural = Zero
> = N extends ""
  ? R
  : N extends `${infer D}0`
  ? FromString<D, Mul<M, Ten>, Mul<M, R>>
  : N extends `${infer D}1`
  ? FromString<D, Mul<M, Ten>, Add<Mul<M, One>, R>>
  : N extends `${infer D}2`
  ? FromString<D, Mul<M, Ten>, Add<Mul<M, Two>, R>>
  : N extends `${infer D}3`
  ? FromString<D, Mul<M, Ten>, Add<Mul<M, Three>, R>>
  : N extends `${infer D}4`
  ? FromString<D, Mul<M, Ten>, Add<Mul<M, Four>, R>>
  : N extends `${infer D}5`
  ? FromString<D, Mul<M, Ten>, Add<Mul<M, Five>, R>>
  : N extends `${infer D}6`
  ? FromString<D, Mul<M, Ten>, Add<Mul<M, Six>, R>>
  : N extends `${infer D}7`
  ? FromString<D, Mul<M, Ten>, Add<Mul<M, Seven>, R>>
  : N extends `${infer D}8`
  ? FromString<D, Mul<M, Ten>, Add<Mul<M, Eight>, R>>
  : N extends `${infer D}9`
  ? FromString<D, Mul<M, Ten>, Add<Mul<M, Nine>, R>>
  : never;

type _00600 = Assert<EQ_<"0", ToString<Zero>>>;
type _00601 = Assert<EQ_<"3", ToString<Three>>>;
type _00602 = Assert<EQ_<"9", ToString<Nine>>>;

type _00603 = Assert<EQ_<Zero, FromString<"0">>>;
type _00604 = Assert<EQ_<One, FromString<"1">>>;
type _00605 = Assert<EQ_<"42", ToString<FromString<"42">>>>;
type _00606 = Refute<EQ_<"69", ToString<FromString<"42">>>>;
type _00607 = Assert<EQ_<"1256", ToString<FromString<"1256">>>>;
type _00608 = Assert<EQ_<"42", ToString<Add<Six, Mul<Nine, Four>>>>>;
type _00609 = Assert<EQ_<FromString<"42">, Add<Six, Mul<Nine, Four>>>>;

// Finally FizzBuzz
type FizzBuzz<N extends Natural> = N extends Zero
  ? never
  : EQ<Mod<N, FromString<"15">>, Zero> extends true
  ? "FizzBuzz"
  : EQ<Mod<N, Three>, Zero> extends true
  ? "Fizz"
  : EQ<Mod<N, Five>, Zero> extends true
  ? "Buzz"
  : ToString<N>;

// Surprisingly easy and tail recursive.
type FizzBuzzTo<
  N extends Natural,
  A extends Array<string> = []
> = N extends Zero ? A : FizzBuzzTo<Decrement<N>, [FizzBuzz<N>, ...A]>;

type _070 = Assert<
  EQ<
    [
      "1",
      "2",
      "Fizz",
      "4",
      "Buzz",
      "Fizz",
      "7",
      "8",
      "Fizz",
      "Buzz",
      "11",
      "Fizz",
      "13",
      "14",
      "FizzBuzz"
    ],
    FizzBuzzTo<FromString<"15">>
  >
>;

type _00700 = Assert<IsNever<FizzBuzz<FromString<"0">>>>;
type _00701 = Assert<EQ_<"Fizz", FizzBuzz<FromString<"3">>>>;
type _00702 = Assert<EQ_<"4", FizzBuzz<FromString<"4">>>>;
type _00703 = Assert<EQ_<"Buzz", FizzBuzz<FromString<"5">>>>;
type _00704 = Assert<EQ_<"Fizz", FizzBuzz<FromString<"6">>>>;
type _00705 = Refute<EQ_<"FizzBuzz", FizzBuzz<FromString<"7">>>>;
type _00706 = Refute<EQ_<"FizzBuzz", FizzBuzz<FromString<"8">>>>;
type _00707 = Refute<EQ_<"FizzBuzz", FizzBuzz<FromString<"9">>>>;
type _00708 = Refute<EQ_<"FizzBuzz", FizzBuzz<FromString<"10">>>>;
type _00709 = Refute<EQ_<"FizzBuzz", FizzBuzz<FromString<"11">>>>;
type _00710 = Refute<EQ_<"FizzBuzz", FizzBuzz<FromString<"12">>>>;
type _00711 = Refute<EQ_<"FizzBuzz", FizzBuzz<FromString<"13">>>>;
type _00712 = Refute<EQ_<"FizzBuzz", FizzBuzz<FromString<"14">>>>;
type _00713 = Assert<EQ_<"FizzBuzz", FizzBuzz<FromString<"15">>>>;

type _00714 = Assert<
  EQ_<FromString<"100">, Count<FizzBuzzTo<FromString<"100">>>>
>;
type _00715 = Assert<EQ_<"1", FirstA<FizzBuzzTo<FromString<"100">>>>>;
type _00716 = Assert<EQ_<"Buzz", LastA<FizzBuzzTo<FromString<"100">>>>>;
type _00717 = FizzBuzzTo<FromString<"100">>;

// Factorial because why not :-D
type Factorial<N extends Natural, A extends Natural = One> = N extends Zero
  ? A
  : Factorial<Decrement<N>, Mul<N, A>>;

type _00800 = Assert<EQ_<One, Factorial<Zero>>>;
type _00801 = Assert<EQ_<One, Factorial<One>>>;
type _00802 = Assert<EQ_<Two, Factorial<Two>>>;
type _00803 = Assert<EQ_<Six, Factorial<Three>>>;
type _00804 = Assert<EQ_<FromString<"24">, Factorial<Four>>>;
type _00805 = Assert<EQ_<FromString<"120">, Factorial<Five>>>;
type _00806 = Assert<EQ_<FromString<"720">, Factorial<Six>>>;
// NOTE: even if it's tail recursive we have a limit of 1000 "recursions"
// type _00807 = Assert<EQ_<FromString<"5040">, Factorial<Seven>>>;
