module Uebung1 where
import Prelude
import RSA
import Data.Char (ord, chr)

{-
Aufgabe 1.1
-}

type Key =(Integer, Integer)

{-
getter für erster Integer des Keys
-}
fstFromInteger :: Key-> Integer
fstFromInteger (e, k) = e

{-
getter für zweiter Integer des Keys
-}
sndFromInteger :: Key-> Integer
sndFromInteger (e, k) = k

{-
encoding(e;k)(m) = m^e mod k;
m= message
e= fstFromInteger publicKey
k= sndFromInteger publicKey
-}
encode :: Key-> Integer-> Integer
encode publicKey m = modExp m (fstFromInteger publicKey) (sndFromInteger publicKey)

{-
decoding(d;k)(c) = c^d mod k:
c= Codierte Zahl
d= fstFromInteger privateKey
k= sndFromInteger privateKey
-}
decode :: Key-> Integer-> Integer
decode privateKey c = modExp c (fstFromInteger privateKey) (sndFromInteger privateKey)

{-
modExp a 1 p wir fangen damit den Fall ab, dass n = 0 ist und somit a^0 = 1 gesetzt wird.
-}
modExp :: Integer-> Integer-> Integer-> Integer
modExp a 0 p = 1
modExp a 1 p = mod a p
modExp a n p =
	if(even n) 
		then mod ((modExp a (div n 2) p)^2) p
		else mod (a*(modExp a (div n 2) p)^2) p
		
{-
Verschlüsselung von Strings zu [Integer] mit Verwendung der Keys
-}
encryption :: Key-> String-> [Integer]
encryption publicKey s = map (encode publicKey) (map toInteger (map ord s))

{-
Entschlüsselung mit Hilfe des PrivateKeys
-}
decryption :: Key -> [Integer]-> String
decryption privateKey listInteger = map chr (map fromInteger (map (decode privateKey) listInteger))


{-
Aufgabe 1.3
-}

-- Zeichenketten in Teilketten der Länge n aufspalten
chunks :: Int-> String-> [String]
chunks n s = if null s then [] else take n s : chunks n (drop n s)



conversionToBase :: Int-> Integer-> [Integer]


conversionFromBase :: Int-> [Integer]-> Integer


stringAsNumber :: String -> Integer


numberAsString :: Integer-> String