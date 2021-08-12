# Week 05

Video: [Iteration #2 Lecture #5](https://youtu.be/SsaVjSsPPcg)

====

### Value
With each (E)UTxO, there is a `Address` and a `Value` (and a `Datum` with
EUTxO). The `Value` has always been ADA (Lovelace).

Tokens are identified by its `CurrencySymbol` and `TokenName` (ADA has both of
these as empty strings). Both of these tokens are a list in `AssetClass`
```hs
newtype AssetClass = AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }
```

Setting the integer value in the `Value` type says how many units of each
`AssetClass` are in the value.
```hs
newtype Value = Value { getValue :: Map.Map CurrencySymbol (Map.Map TokenName Integer) }
```

`singleton` can be used to create a new token `Value` other than ADA.
```hs
singleton :: CurrencySymbol -> TokenName -> Integer -> Value
```

Example:
```bash
ghci> singleton "a8ff" "MY_TOKEN" 10
Value (Map [(a8ff,Map [("MY_TOKEN",10)])])
```

### Combining Value's
`Value` class is an instance of `Moniod`, which means it can be used with the
`mappend` (`<>`) functionality that Moniod gives to combine 2+ `Value`s

Combining two `lovelaceValueOf` values together will give a single `Value`
```hs
lovelaceValueOf 15 <> lovelaceValueOf 10
```
which gives the `Value` of 25 lovelaces. This only works since the
`CurrencySymbol` are the same for both lovelace. Having different
`CurrencySymbol` will return a map of the `Value`'s.
```bash
ghci> singleton "a8ff" "ABC" 7 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 1000
Value (Map [(, Map [("", 42)]), ("a8ff", Map [("ABC", 7), ("XYZ", 100)])])
```
This can be read as
```json
{
    "": {
        "": 42
    }, {
    "a8ff": {
        "ABC": 7,
        "XYZ": 100
    }
}
```

To read the `"XYZ"` value of `"a8ff"`, `valueOf` function will retrieve it
```hs
valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
```
```bash
ghci> let v = singleton "a8ff" "ABC" 7 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 1000
ghci> valueOf v "a8ff" "XYZ"
100
```

### Flatten Value
`flattenValue` will flatten the `Maps` in our `Value` to only show the list
rather than the `Maps`
```bash
ghci> flattenValue v
[(a8ff,"ABC",7),(a8ff,"XYZ",100),(,"",42)]
```

### Minting
Minting requires a `CurrencySymbol` to have a hash of a script. This is the
reason as to why the `CurrencySymbol` is hexadecimal. This is the Minting
Policy (`MonetaryPolicy`).

For each transaction that wants to mint or burn native tokens, `CurrencySymbol`
is looked-up via the hash value. Once the `CurrencySymbol` is found, the script
is ran alongside other validation scripts. If all is good, the transaction is
allowed to mint/burn native tokens.

Since ADA is an empty string, there is no script to be ran, which means that
there is no way of minting or burning ADA. The amount of ADA is fixed and
cannot be altered.

For the past couple of weeks, the only purpose of our script (`ScriptPurpose`)
was `Spending`, but there is another purpose as well, `Minting`.
```hs
-- ScriptContext (what has been used for the past weeks) uses ScriptPurpose
data ScriptContext =
    ScriptContext {
        scriptContextTxInfo :: TxInfo,
        scriptContextPurpose :: ScriptPurpose
    }

-- Where ScriptPurpose is defined as
data ScriptPurpose
    = Minting CurrencySymbol
    | Spending TxOutRef
    | Rewarding StakingCredential
    | Certifying DCert
```

### Forging
Also inside of `ScriptContext`, the `TxInfo` is used
```hs
data TxInfo = TxInfo {
    txInfoInputs      :: [TxInInfo] -- ^ Transaction inputs,
    txInfoOutputs     :: [TxOut] -- ^ Transaction outputs,
    txInfoFee         :: Value -- ^ The fee paid by this transaction,
    txInfoForge       :: Value -- ^ The 'Value' forged by this transaction,
    txInfoDCert       :: [DCert] -- ^ Digests of certificates included in this transaction,
    txInfoWdrl        :: [(StakingCredential, Integer)] -- ^ Withdrawals,
    txInfoValidRange  :: SlotRange -- ^ The valid range for the transaction,
    txInfoSignatories :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx,
    txInfoData        :: [(DatumHash, Datum)],
    txInfoId          :: TxId
    -- ^ Hash of the pending transaction (excluding witnesses)
} deriving (Generic)
```
Up until now, the `txInfoForge` has always been zero (0); though for the
Minting Policy, this may contain a `Value` with 1 or more `AssetClass`

For each `AssetClass` in `txInfoForge`, the corresponding script is ran
(`Minting Policy` or `Montery Policy`).

In [Week03](https://github.com/loerac/plutus-pioneer-program/tree/main/Week03),
the validation script has 3 parameters: Datum, Redeemer, and ScriptContext. But
with Minting Policies, only the ScriptContext is to be used.

This is due to Datum and Redeemer belongs to a UTxO and input respectively.
Forging (`txInfoForge`) belongs to the transaction, not a specific IO.

### Non-Fungible Token (NFT)
NFTs are tokens that can exist only once, there is only one of each in existence.

Two options to go about creating a NFT:
#### Option #1
Using the policy in Forge Field, `txInfoForge`, there could be an added
condition that only one token is forged. But that won't do any good since that
would mean during one transaction, it could only mint one token. This wouldn't
stop it from doing multiple transactions.

#### Option #2
When creating a NFT, set a deadline in which nobody is allowed to mint or burn any more tokens. This would mint 1, wait until the deadline has passed, and then a NFT is born.

One issue with this approach is to check if the NFT is really an NFT, one would have to use the blockchain explorer to check if it's in the blockchain, which isn't easy.
