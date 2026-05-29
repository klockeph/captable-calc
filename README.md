# captable-calc

A small CLI that helps option holders at startups figure out what their equity is actually worth at different exit prices — accounting for liquidation preferences across financing rounds.

Given a YAML description of the cap table (financing rounds, your owned lots, and which lots you've already exercised), it answers:

- At a given exit, what are my gross proceeds and net profit?
- What's the minimum exit price where I make positive profit?
- How does my outcome vary across a range of exit scenarios?

Not financial advice — just arithmetic.

## Caveats

- **Liquidation model**: 1× non-participating preferred only. No participating preferred, no multipliers >1×, no liquidation caps, no anti-dilution.
- **No taxes**: Output is pre-tax everywhere. Tax treatment is jurisdiction-specific (US NSO/ISO + capital gains, NL Box 3 wealth tax vs. income on exit, etc.) and intentionally out of scope.
- **FMV = strike**: Each lot's `fmv` field is treated as the strike price you'd pay to exercise. This holds for options granted at FMV per 409A, which is the common case.

## Build

With cabal:

```
cabal run captable-calc -- config.yaml
```

Or via the bundled Nix flake (`nix develop`, then `cabal run …`).

Run the tests with `cabal test`.

## Config

```yaml
financingRounds:
  - name: "Seed"
    fullyDiluted: 20            # total fully-diluted shares after this round
    sharesIssued: 5             # preferred shares issued in this round
    issuePrice: 1.23            # price per share paid by investors
  - name: "Series A"
    fullyDiluted: 200
    sharesIssued: 100
    issuePrice: 3.14

ownedShares:
  - amount: 1
    fmv: 1.23
    exercised: true             # strike already paid; loss possible if underwater
  - amount: 2
    fmv: 3.14                   # `exercised` omitted -> defaults to false
```

`fullyDiluted` is post-round and should include the option pool. The most recent round's value is taken as the current total.

## Usage

### Inflection points (no price argument)

```
$ captable-calc config.yaml
Strike paid (exercised lots): $1.23

Inflection Points:
  Not exercised:       $437.00
  Exercised as config: $437.01
  All exercised:       $589.80
```

Three break-even exit prices — where total profit first turns positive — under different assumptions:

- **Not exercised**: none of your lots have been exercised yet.
- **Exercised as config**: uses the `exercised` flag per lot.
- **All exercised**: assumes you've already paid strike on every lot.

### Sensitivity table (one or more prices)

Prices accept K/M/B/T suffixes:

```
$ captable-calc config.yaml 1K 10K 100K
Strike paid (exercised lots): $1.23

Exit Price  Proceeds  Profit
$1.00K      $25.00    $11.21
$10.00K     $250.00   $236.21
$100.00K    $2.50K    $2.49K
```

`Proceeds` is gross pro-rata sale value at the given exit. `Profit` uses the `exercised` flag on each lot — exercised lots can lose money when underwater; non-exercised lots are capped at 0 (you'd simply not exercise).

### Extensive view (`-e`)

`--extensive` / `-e` shows all three Profit perspectives side by side:

```
$ captable-calc config.yaml -e 1K 10K
Strike paid (exercised lots): $1.23

Exit Price  Proceeds  Profit (unex.)  Profit (as cfg)  Profit (exer.)
$1.00K      $25.00    $11.21          $11.21           $11.21
$10.00K     $250.00   $236.21         $236.21          $236.21
```
