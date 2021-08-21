[![https://jappieklooster.nl](https://img.shields.io/badge/blog-jappieklooster.nl-lightgrey)](https://jappieklooster.nl/tag/haskell.html)
[![Jappiejappie](https://img.shields.io/badge/twitch.tv-jappiejappie-purple?logo=twitch)](https://www.twitch.tv/jappiejappie)
[![Jappiejappie](https://img.shields.io/badge/discord-jappiejappie-black?logo=discord)](https://discord.gg/Hp4agqy)

> Three hundred and twenty years have passed since the coven sank in the dark. Phantom from the past. Time that started to move again.

Removes the magic from witch.
This provides safe conversions like witch does.
But it doesn't use type classes or exceptions.
This has a couple of advantages:

1. No need to use type application for function selection.
2. Functions get names that describe what they do.
   This allows ctags to work as well.
3. No trouble with orphans.
4. Custom errors instead of the prelude based ones allow client
   code to recover with typesafety even on partial conversions.

   
## Usage

```haskell
import qualified Unwitch.Convert.Double as Double

main = do
  Double.toInteger 5.6 `shouldBe` Left $ RationalConversion $ DenomNotOne (5 % 6)
```

Furthermore if the client can figure out how to go on with a Rational,
they can.
It's not an exception like witch implied.

### Tools
Enter the nix shell.
```
nix-shell
```
You can checkout the makefile to see what's available:
```
cat makefile
```

### Running
```
make run
```

### Fast filewatch which runs tests
```
make ghcid
```
