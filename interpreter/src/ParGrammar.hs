{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParGrammar where
import AbsGrammar
import LexGrammar
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn32 (Ident)
	| HappyAbsSyn33 (Integer)
	| HappyAbsSyn34 (UIdent)
	| HappyAbsSyn35 (LIdent)
	| HappyAbsSyn36 ([TopDef])
	| HappyAbsSyn37 (Program)
	| HappyAbsSyn38 (TopDef)
	| HappyAbsSyn39 (VDef)
	| HappyAbsSyn40 ([Ident])
	| HappyAbsSyn41 ([MCase])
	| HappyAbsSyn42 (Exp)
	| HappyAbsSyn43 (MCase)
	| HappyAbsSyn44 ([Bind])
	| HappyAbsSyn45 (Bind)
	| HappyAbsSyn48 ([VDef])
	| HappyAbsSyn53 ([Exp])
	| HappyAbsSyn55 ([LIdent])
	| HappyAbsSyn56 ([SubType])
	| HappyAbsSyn57 ([Type])
	| HappyAbsSyn59 (TDef)
	| HappyAbsSyn60 (Type)
	| HappyAbsSyn64 (SubType)

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x2c\x01\x2c\x01\x2c\x01\xd6\x01\xd6\x01\xd4\x01\x1e\x00\xd4\x01\x3e\x00\x8b\x00\x00\x00\x3e\x00\x00\x00\x38\x00\x38\x00\x38\x00\x38\x00\x1e\x00\x38\x00\xd2\x01\x00\x00\x0a\x00\x00\x00\x2c\x01\x10\x00\x10\x00\x0a\x00\x0a\x00\xd0\x01\xd3\x01\x00\x00\xce\x01\xc6\x01\x00\x00\x00\x00\x00\x00\xda\x01\x00\x00\xbc\x01\x0a\x00\x10\x00\xc0\x01\x00\x00\x00\x00\xb4\x01\x30\x00\xb4\x01\xc3\x01\xa7\x01\xae\x01\x07\x00\xae\x01\xbe\x01\x2b\x01\x8d\x01\x9d\x01\x00\x00\x00\x00\x00\x00\x9d\x01\x1e\x00\x1e\x00\x00\x00\xa3\x01\xdb\x00\xfe\xff\xe1\x00\x35\x00\x8e\x01\x00\x00\x8f\x01\x1e\x00\x8b\x01\x1e\x00\x04\x00\x7f\x01\x98\x01\x79\x00\x03\x00\xef\xff\x00\x00\x00\x00\x00\x00\x8c\x01\x6e\x01\x2a\x00\x2a\x00\x00\x00\xff\xff\x6e\x01\x6e\x01\x7a\x01\x68\x01\x2a\x00\x68\x01\x68\x01\x6b\x01\x60\x01\x62\x01\x00\x00\x62\x01\x6a\x01\x5d\x01\x00\x00\x00\x00\x00\x00\x5d\x01\x61\x01\x57\x01\x2c\x01\x7f\x00\x0a\x00\x00\x00\x00\x00\x65\x01\x2a\x00\x00\x00\x5b\x01\x66\x01\x8b\x00\x8b\x00\x5c\x01\x38\x00\x38\x00\x38\x00\x00\x00\x4a\x01\x00\x00\x52\x01\x5a\x01\x38\x00\x38\x00\x38\x00\x38\x00\x38\x00\x38\x00\x1e\x00\x51\x01\x4b\x01\x00\x00\x00\x00\x0a\x00\x00\x00\x49\x01\x10\x00\x00\x00\x77\x00\x42\x01\x10\x00\x00\x00\x10\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x01\x00\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x1e\x00\x46\x00\x13\x01\x35\x01\x35\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x0c\x01\x1e\x00\xfd\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x00\x00\xdc\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xbb\x00\xab\x00\xbe\x00\x29\x01\x69\x00\x26\x01\x81\x01\xed\x00\x0b\x02\x4f\x01\xe5\x00\x1e\x02\xd0\x00\xaa\x01\x87\x01\xec\x01\x82\x00\x07\x01\x12\x01\xa1\x00\xc3\x00\x5e\x00\xb6\x00\xb4\x00\xc7\x00\xc5\x00\x83\x00\x76\x00\xa3\x00\x00\x00\x00\x00\x00\x00\xaa\x00\x75\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x57\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8e\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x52\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\x00\xea\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf5\x00\x00\x00\x00\x00\x0f\x00\x7b\x01\x00\x00\x64\x01\xf5\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa7\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x07\x02\xf7\x01\x00\x00\x32\x01\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x02\x00\x00\x00\x00\x01\x01\x02\x00\x00\x00\x28\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb3\x00\x32\x01\x6c\x00\x00\x00\x00\x00\x00\x00\xcb\x00\x00\x00\x00\x00\x00\x00\x15\x01\xd1\x00\x00\x00\xe8\x01\xa6\x01\xa2\x01\x00\x00\x00\x00\x0b\x00\x00\x00\x00\x00\xd1\x01\xcd\x01\xc9\x01\xc5\x01\x9e\x01\xc1\x01\xd3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x49\x00\x00\x00\x00\x00\x43\x00\x2b\x00\x00\x00\x00\x00\x3f\x00\xf1\xff\x3f\x00\x00\x00\x00\x00\xe8\xff\x00\x00\x4e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5e\x01\x47\x01\xa7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x01\x00\x00\x2a\x01\x00\x00\x00\x00\x00\x00\x9d\x00\x24\x01\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcc\xff\x00\x00\xc4\xff\x00\x00\xbf\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb1\xff\x00\x00\xa9\xff\xa7\xff\xa5\xff\xa2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe2\xff\x00\x00\x00\x00\xa2\xff\x9d\xff\x99\xff\x97\xff\x96\xff\x00\x00\xa5\xff\x00\x00\x00\x00\xe0\xff\xdf\xff\x00\x00\x00\x00\x00\x00\x00\x00\xa9\xff\x00\x00\x00\x00\x00\x00\xa4\xff\x00\x00\xa9\xff\x00\x00\xaa\xff\xac\xff\xab\xff\x00\x00\xb1\xff\xb1\xff\xe1\xff\xb0\xff\xb6\xff\xce\xff\xb9\xff\xb4\xff\x00\x00\xb2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb6\xff\x00\x00\x00\x00\x00\x00\xc7\xff\xc6\xff\xc4\xff\xc0\xff\x00\x00\xcc\xff\xcc\xff\xc5\xff\x00\x00\x00\x00\x00\x00\xcb\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd6\xff\xd8\xff\x00\x00\xc4\xff\x00\x00\xc4\xff\x00\x00\xdb\xff\xda\xff\xdc\xff\x00\x00\x00\x00\x00\x00\xde\xff\x00\x00\x00\x00\xd7\xff\xd5\xff\x00\x00\xcc\xff\xc3\xff\x00\x00\x00\x00\x00\x00\xc1\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb3\xff\x00\x00\xbf\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb1\xff\x00\x00\x00\x00\xa8\xff\xa6\xff\xa5\xff\xa1\xff\x00\x00\x00\x00\xa7\xff\x00\x00\x00\x00\x9e\xff\xa2\xff\x95\xff\x9c\xff\x9b\xff\x00\x00\x9a\xff\x00\x00\xa3\xff\xad\xff\xae\xff\xaf\xff\xd0\xff\xcf\xff\xbc\xff\xbb\xff\xbd\xff\xb5\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb7\xff\xb8\xff\xba\xff\xbe\xff\xc2\xff\xc8\xff\xc9\xff\xca\xff\x00\x00\x9f\xff\x00\x00\xdd\xff\xd9\xff\xcd\xff\x00\x00\x00\x00\x00\x00\xd1\xff\xa0\xff\x98\xff\x00\x00\xd3\xff\x00\x00\xd4\xff\xd2\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x00\x00\x05\x00\x03\x00\x07\x00\x02\x00\x04\x00\x20\x00\x02\x00\x08\x00\x1a\x00\x02\x00\x1e\x00\x0f\x00\x00\x00\x11\x00\x22\x00\x02\x00\x0f\x00\x02\x00\x03\x00\x0f\x00\x08\x00\x17\x00\x0f\x00\x1c\x00\x10\x00\x12\x00\x1e\x00\x1f\x00\x0f\x00\x02\x00\x22\x00\x1e\x00\x1f\x00\x20\x00\x22\x00\x22\x00\x20\x00\x21\x00\x22\x00\x20\x00\x21\x00\x02\x00\x0f\x00\x1c\x00\x1d\x00\x20\x00\x21\x00\x14\x00\x15\x00\x16\x00\x17\x00\x0e\x00\x02\x00\x08\x00\x0f\x00\x02\x00\x11\x00\x1e\x00\x1f\x00\x20\x00\x0a\x00\x02\x00\x02\x00\x03\x00\x18\x00\x0f\x00\x02\x00\x03\x00\x0f\x00\x1e\x00\x1f\x00\x20\x00\x02\x00\x03\x00\x0f\x00\x0e\x00\x11\x00\x02\x00\x03\x00\x22\x00\x1e\x00\x1f\x00\x20\x00\x1e\x00\x1f\x00\x20\x00\x02\x00\x03\x00\x1c\x00\x1e\x00\x1f\x00\x20\x00\x1c\x00\x02\x00\x03\x00\x19\x00\x1d\x00\x1e\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x00\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x02\x00\x03\x00\x19\x00\x08\x00\x20\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x19\x00\x02\x00\x03\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x05\x00\x08\x00\x07\x00\x02\x00\x00\x00\x01\x00\x02\x00\x02\x00\x03\x00\x10\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0d\x00\x02\x00\x0f\x00\x1a\x00\x11\x00\x03\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x14\x00\x1b\x00\x16\x00\x1d\x00\x0f\x00\x22\x00\x11\x00\x1e\x00\x1f\x00\x1c\x00\x1d\x00\x1e\x00\x02\x00\x03\x00\x03\x00\x17\x00\x09\x00\x00\x00\x0b\x00\x1e\x00\x1f\x00\x00\x00\x02\x00\x02\x00\x07\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x02\x00\x02\x00\x04\x00\x17\x00\x06\x00\x07\x00\x00\x00\x1c\x00\x02\x00\x00\x00\x04\x00\x02\x00\x06\x00\x07\x00\x20\x00\x06\x00\x07\x00\x1b\x00\x02\x00\x03\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x1b\x00\x1b\x00\x1a\x00\x00\x00\x01\x00\x00\x00\x01\x00\x02\x00\x1b\x00\x0c\x00\x0d\x00\x1b\x00\x0f\x00\x18\x00\x01\x00\x0a\x00\x0d\x00\x04\x00\x10\x00\x1c\x00\x1d\x00\x1c\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x0c\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x0e\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x0b\x00\x1d\x00\x0a\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x09\x00\x16\x00\x0b\x00\x00\x00\x01\x00\x02\x00\x13\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x00\x00\x01\x00\x0a\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x11\x00\x12\x00\x13\x00\x14\x00\x0d\x00\x16\x00\x00\x00\x01\x00\x02\x00\x1b\x00\x16\x00\x00\x00\x00\x00\x01\x00\x02\x00\x1a\x00\x0a\x00\x09\x00\x07\x00\x0b\x00\x00\x00\x01\x00\x0a\x00\x11\x00\x12\x00\x13\x00\x14\x00\x04\x00\x16\x00\x11\x00\x12\x00\x13\x00\x14\x00\x0d\x00\x16\x00\x00\x00\x01\x00\x02\x00\x04\x00\x03\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x1e\x00\x0a\x00\x20\x00\x22\x00\x03\x00\x00\x00\x01\x00\x0a\x00\x11\x00\x12\x00\x13\x00\x14\x00\x0d\x00\x16\x00\x11\x00\x12\x00\x13\x00\x14\x00\x0d\x00\x16\x00\x00\x00\x01\x00\x02\x00\x10\x00\x08\x00\x19\x00\x00\x00\x01\x00\x02\x00\x0b\x00\x0a\x00\x03\x00\x18\x00\x10\x00\x0b\x00\x08\x00\x0a\x00\x11\x00\x12\x00\x13\x00\x14\x00\x09\x00\x16\x00\x11\x00\x12\x00\x13\x00\x14\x00\x22\x00\x16\x00\x00\x00\x01\x00\x02\x00\x1e\x00\x22\x00\x06\x00\x00\x00\x01\x00\x02\x00\x22\x00\x0a\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x22\x00\x0a\x00\x11\x00\x12\x00\x13\x00\x14\x00\x22\x00\x16\x00\x11\x00\x12\x00\x13\x00\x14\x00\x0a\x00\x16\x00\x11\x00\x12\x00\x13\x00\x14\x00\x04\x00\x16\x00\x00\x00\x01\x00\x02\x00\x22\x00\x00\x00\x01\x00\x02\x00\x1a\x00\x00\x00\x01\x00\x02\x00\x06\x00\x00\x00\x01\x00\x02\x00\x1e\x00\x21\x00\x11\x00\x22\x00\x13\x00\x14\x00\x11\x00\x16\x00\x13\x00\x14\x00\x11\x00\x16\x00\x13\x00\x14\x00\x11\x00\x16\x00\x13\x00\x14\x00\x22\x00\x16\x00\x00\x00\x01\x00\x02\x00\x06\x00\x00\x00\x01\x00\x02\x00\x21\x00\x00\x00\x01\x00\x02\x00\x09\x00\x00\x00\x01\x00\x02\x00\x22\x00\x00\x00\x01\x00\x02\x00\x13\x00\x14\x00\x22\x00\x16\x00\x13\x00\x14\x00\x1a\x00\x16\x00\x13\x00\x14\x00\x22\x00\x16\x00\x13\x00\x14\x00\x08\x00\x16\x00\x13\x00\x14\x00\x20\x00\x16\x00\x00\x00\x01\x00\x02\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x1b\x00\x22\x00\x1e\x00\xff\xff\x21\x00\x1e\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x13\x00\x14\x00\xff\xff\x16\x00\x13\x00\x14\x00\xff\xff\x16\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x00\x00\x01\x00\x02\x00\xff\xff\x00\x00\x01\x00\x02\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x0c\x00\x0d\x00\xff\xff\x0f\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x0d\x00\xff\xff\x0f\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\x0f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x56\x00\x61\x00\x7c\x00\x36\x00\x7d\x00\x3d\x00\x7b\x00\x8c\x00\x28\x00\x70\x00\x96\x00\x28\x00\x1f\x00\x57\x00\x61\x00\x58\x00\xff\xff\x28\x00\x3e\x00\x21\x00\x22\x00\x29\x00\x81\x00\x8b\x00\x29\x00\x87\x00\xa8\x00\x2a\x00\x1f\x00\x3f\x00\x29\x00\x3d\x00\xff\xff\x1f\x00\x3f\x00\x2b\x00\xff\xff\xff\xff\x2b\x00\x2c\x00\xff\xff\x2b\x00\x2c\x00\x56\x00\x3e\x00\x23\x00\x92\x00\x2b\x00\x2c\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x6e\x00\x3d\x00\x91\x00\x57\x00\x3d\x00\x58\x00\x1f\x00\x3f\x00\x2b\x00\x83\x00\x56\x00\x21\x00\x22\x00\x99\x00\x3e\x00\x21\x00\x22\x00\x3e\x00\x1f\x00\x3f\x00\x2b\x00\x21\x00\x22\x00\x57\x00\x78\x00\x58\x00\x21\x00\x22\x00\xff\xff\x1f\x00\x3f\x00\x2b\x00\x1f\x00\x3f\x00\x2b\x00\x21\x00\x22\x00\x8e\x00\x1f\x00\x3f\x00\x2b\x00\x9a\x00\x21\x00\x22\x00\x9c\x00\xba\x00\x1f\x00\x23\x00\x24\x00\x25\x00\x34\x00\x61\x00\x23\x00\x24\x00\x25\x00\xbc\x00\x21\x00\x22\x00\x93\x00\x62\x00\x8c\x00\x23\x00\x24\x00\x25\x00\x34\x00\x33\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x34\x00\x7c\x00\x91\x00\x7d\x00\x56\x00\x38\x00\x39\x00\x3a\x00\x21\x00\x22\x00\x99\x00\x23\x00\x24\x00\x25\x00\xb3\x00\xb5\x00\x56\x00\x57\x00\x94\x00\x58\x00\x36\x00\x23\x00\x24\x00\x25\x00\x26\x00\x4a\x00\x21\x00\x45\x00\xbe\x00\x57\x00\xff\xff\x58\x00\x1f\x00\x3f\x00\x23\x00\x24\x00\x2c\x00\x21\x00\x22\x00\x36\x00\x8f\x00\xc0\x00\x63\x00\x60\x00\x1f\x00\x3f\x00\x65\x00\x95\x00\x30\x00\x79\x00\x69\x00\x6a\x00\x6b\x00\x67\x00\x65\x00\x2f\x00\x30\x00\x30\x00\xb5\x00\x37\x00\x6b\x00\x67\x00\x65\x00\x8e\x00\x30\x00\x65\x00\x6c\x00\x30\x00\x6b\x00\x67\x00\x1f\x00\x66\x00\x67\x00\x68\x00\x21\x00\x22\x00\x21\x00\x22\x00\x50\x00\x51\x00\x52\x00\x68\x00\x31\x00\x32\x00\x50\x00\x51\x00\x38\x00\x39\x00\x3a\x00\x68\x00\xb1\x00\x53\x00\x68\x00\x5b\x00\x35\x00\x88\x00\x3f\x00\x74\x00\x7b\x00\x4f\x00\x23\x00\x2d\x00\x2e\x00\x40\x00\x41\x00\x42\x00\x43\x00\x9f\x00\x45\x00\x38\x00\x39\x00\x3a\x00\x84\x00\x85\x00\x86\x00\x38\x00\x39\x00\x3a\x00\x58\x00\x3f\x00\x38\x00\x39\x00\x3a\x00\x5c\x00\xc2\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x89\x00\x45\x00\x40\x00\x41\x00\x42\x00\x43\x00\x8a\x00\x45\x00\x38\x00\x39\x00\x3a\x00\x71\x00\x7d\x00\x60\x00\x38\x00\x39\x00\x3a\x00\xbf\x00\x3f\x00\x38\x00\x39\x00\x3a\x00\x50\x00\x51\x00\xc2\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x40\x00\x41\x00\x42\x00\x43\x00\xae\x00\x45\x00\x38\x00\x39\x00\x3a\x00\x5e\x00\x3b\x00\x63\x00\x38\x00\x39\x00\x3a\x00\xb9\x00\xbf\x00\x5f\x00\x64\x00\x60\x00\x50\x00\x51\x00\xb6\x00\x40\x00\x41\x00\x42\x00\x43\x00\x7b\x00\x45\x00\x40\x00\x41\x00\x42\x00\x43\x00\x74\x00\x45\x00\x38\x00\x39\x00\x3a\x00\x7b\x00\x98\x00\x21\x00\x38\x00\x39\x00\x3a\x00\x1f\x00\xb7\x00\x2b\x00\xff\xff\x9e\x00\x50\x00\x51\x00\xba\x00\x40\x00\x41\x00\x42\x00\x43\x00\x9c\x00\x45\x00\x40\x00\x41\x00\x42\x00\x43\x00\x59\x00\x45\x00\x38\x00\x39\x00\x3a\x00\x9f\x00\xa7\x00\xaa\x00\x38\x00\x39\x00\x3a\x00\xae\x00\xbb\x00\xb0\x00\xa8\x00\xb1\x00\x6e\x00\xb3\x00\x7e\x00\x40\x00\x41\x00\x42\x00\x43\x00\x70\x00\x45\x00\x40\x00\x41\x00\x42\x00\x43\x00\xff\xff\x45\x00\x38\x00\x39\x00\x3a\x00\x1f\x00\xff\xff\x74\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x80\x00\x5e\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x5e\x00\x40\x00\x41\x00\x42\x00\x43\x00\xff\xff\x45\x00\x40\x00\x41\x00\x42\x00\x43\x00\x78\x00\x45\x00\x4c\x00\x4d\x00\x42\x00\x43\x00\x7b\x00\x45\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x38\x00\x39\x00\x3a\x00\x80\x00\x38\x00\x39\x00\x3a\x00\x89\x00\x38\x00\x39\x00\x3a\x00\x1f\x00\x2c\x00\xa1\x00\xff\xff\x42\x00\x43\x00\xaa\x00\x45\x00\x42\x00\x43\x00\xab\x00\x45\x00\x42\x00\x43\x00\x4e\x00\x45\x00\x42\x00\x43\x00\xff\xff\x45\x00\x38\x00\x39\x00\x3a\x00\x8e\x00\x38\x00\x39\x00\x3a\x00\x2c\x00\x38\x00\x39\x00\x3a\x00\x70\x00\x38\x00\x39\x00\x3a\x00\xff\xff\x38\x00\x39\x00\x3a\x00\xa0\x00\x43\x00\xff\xff\x45\x00\xa2\x00\x43\x00\x92\x00\x45\x00\xa3\x00\x43\x00\xff\xff\x45\x00\xa4\x00\x43\x00\x91\x00\x45\x00\xa5\x00\x43\x00\x2b\x00\x45\x00\x38\x00\x39\x00\x3a\x00\x21\x00\x38\x00\x39\x00\x3a\x00\x5e\x00\xff\xff\x1f\x00\x00\x00\x2c\x00\x1f\x00\x00\x00\x00\x00\x50\x00\x51\x00\x52\x00\x00\x00\xac\x00\x43\x00\x00\x00\x45\x00\x4b\x00\x43\x00\x00\x00\x45\x00\x75\x00\x53\x00\x00\x00\x5b\x00\x50\x00\x51\x00\x52\x00\x00\x00\x50\x00\x51\x00\x52\x00\x50\x00\x51\x00\x52\x00\x00\x00\x00\x00\x76\x00\x53\x00\x00\x00\x5b\x00\x5a\x00\x53\x00\x00\x00\x5b\x00\x53\x00\x00\x00\x72\x00\x50\x00\x51\x00\x52\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x53\x00\x00\x00\x54\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (29, 106) [
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106)
	]

happy_n_terms = 35 :: Int
happy_n_nonterms = 33 :: Int

happyReduce_29 = happySpecReduce_1  0# happyReduction_29
happyReduction_29 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn32
		 (Ident happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  1# happyReduction_30
happyReduction_30 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn33
		 ((read ( happy_var_1)) :: Integer
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  2# happyReduction_31
happyReduction_31 (HappyTerminal (PT _ (T_UIdent happy_var_1)))
	 =  HappyAbsSyn34
		 (UIdent (happy_var_1)
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  3# happyReduction_32
happyReduction_32 (HappyTerminal (PT _ (T_LIdent happy_var_1)))
	 =  HappyAbsSyn35
		 (LIdent (happy_var_1)
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  4# happyReduction_33
happyReduction_33 _
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn36
		 ((:[]) happy_var_1
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  4# happyReduction_34
happyReduction_34 (HappyAbsSyn36  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn36
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  5# happyReduction_35
happyReduction_35 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn37
		 (AbsGrammar.Prog happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  6# happyReduction_36
happyReduction_36 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn38
		 (AbsGrammar.VarDef happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  6# happyReduction_37
happyReduction_37 (HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn38
		 (AbsGrammar.TypDef happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happyReduce 4# 7# happyReduction_38
happyReduction_38 ((HappyAbsSyn42  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn44  happy_var_2) `HappyStk`
	(HappyAbsSyn32  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (AbsGrammar.Def happy_var_1 (reverse happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_39 = happySpecReduce_1  8# happyReduction_39
happyReduction_39 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn40
		 ((:[]) happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_2  8# happyReduction_40
happyReduction_40 (HappyAbsSyn40  happy_var_2)
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn40
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  9# happyReduction_41
happyReduction_41 (HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn41
		 ((:[]) happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_2  9# happyReduction_42
happyReduction_42 (HappyAbsSyn41  happy_var_2)
	(HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn41
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_42 _ _  = notHappyAtAll 

happyReduce_43 = happyReduce 6# 10# happyReduction_43
happyReduction_43 (_ `HappyStk`
	(HappyAbsSyn41  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn42  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn42
		 (AbsGrammar.EMat happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_44 = happyReduce 5# 10# happyReduction_44
happyReduction_44 ((HappyAbsSyn42  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn42
		 (AbsGrammar.ELet (reverse happy_var_3) happy_var_5
	) `HappyStk` happyRest

happyReduce_45 = happyReduce 6# 10# happyReduction_45
happyReduction_45 ((HappyAbsSyn42  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn42  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn42  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn42
		 (AbsGrammar.EIf happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_46 = happyReduce 4# 10# happyReduction_46
happyReduction_46 ((HappyAbsSyn42  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn40  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn42
		 (AbsGrammar.ELam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_47 = happySpecReduce_3  10# happyReduction_47
happyReduction_47 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (AbsGrammar.EAnd happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  10# happyReduction_48
happyReduction_48 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (AbsGrammar.EOr happy_var_1 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  10# happyReduction_49
happyReduction_49 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happyReduce 4# 11# happyReduction_50
happyReduction_50 ((HappyAbsSyn42  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn45  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn43
		 (AbsGrammar.MCas happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_51 = happySpecReduce_0  12# happyReduction_51
happyReduction_51  =  HappyAbsSyn44
		 ([]
	)

happyReduce_52 = happySpecReduce_1  12# happyReduction_52
happyReduction_52 (HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn44
		 ((:[]) happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  12# happyReduction_53
happyReduction_53 (HappyAbsSyn44  happy_var_3)
	_
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn44
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  13# happyReduction_54
happyReduction_54 _
	(HappyAbsSyn44  happy_var_2)
	_
	 =  HappyAbsSyn45
		 (AbsGrammar.BLis happy_var_2
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  13# happyReduction_55
happyReduction_55 _
	(HappyAbsSyn44  happy_var_2)
	_
	 =  HappyAbsSyn45
		 (AbsGrammar.BTup happy_var_2
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  13# happyReduction_56
happyReduction_56 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn45
		 (AbsGrammar.BVar happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  13# happyReduction_57
happyReduction_57 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn45
		 (AbsGrammar.BInt happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  13# happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn45
		 (AbsGrammar.BSkip
	)

happyReduce_59 = happySpecReduce_0  14# happyReduction_59
happyReduction_59  =  HappyAbsSyn44
		 ([]
	)

happyReduce_60 = happySpecReduce_2  14# happyReduction_60
happyReduction_60 (HappyAbsSyn45  happy_var_2)
	(HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn44
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_60 _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  15# happyReduction_61
happyReduction_61 (HappyAbsSyn45  happy_var_3)
	_
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn45
		 (AbsGrammar.BULis happy_var_1 happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_2  15# happyReduction_62
happyReduction_62 (HappyAbsSyn44  happy_var_2)
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn45
		 (AbsGrammar.BCon happy_var_1 (reverse happy_var_2)
	)
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  15# happyReduction_63
happyReduction_63 (HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn45
		 (happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_0  16# happyReduction_64
happyReduction_64  =  HappyAbsSyn48
		 ([]
	)

happyReduce_65 = happySpecReduce_3  16# happyReduction_65
happyReduction_65 _
	(HappyAbsSyn39  happy_var_2)
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  17# happyReduction_66
happyReduction_66 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (AbsGrammar.ESmal happy_var_1 happy_var_3
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  17# happyReduction_67
happyReduction_67 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (AbsGrammar.ESmal happy_var_1 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  17# happyReduction_68
happyReduction_68 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (AbsGrammar.EEq happy_var_1 happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  17# happyReduction_69
happyReduction_69 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (AbsGrammar.EMul happy_var_1 happy_var_3
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  17# happyReduction_70
happyReduction_70 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  18# happyReduction_71
happyReduction_71 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (AbsGrammar.EAdd happy_var_1 happy_var_3
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  18# happyReduction_72
happyReduction_72 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (AbsGrammar.ESub happy_var_1 happy_var_3
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  18# happyReduction_73
happyReduction_73 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  19# happyReduction_74
happyReduction_74 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (AbsGrammar.ECons happy_var_1 happy_var_3
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  19# happyReduction_75
happyReduction_75 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_2  20# happyReduction_76
happyReduction_76 (HappyAbsSyn42  happy_var_2)
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (AbsGrammar.EApp happy_var_1 happy_var_2
	)
happyReduction_76 _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  20# happyReduction_77
happyReduction_77 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_1
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_0  21# happyReduction_78
happyReduction_78  =  HappyAbsSyn53
		 ([]
	)

happyReduce_79 = happySpecReduce_1  21# happyReduction_79
happyReduction_79 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn53
		 ((:[]) happy_var_1
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_3  21# happyReduction_80
happyReduction_80 (HappyAbsSyn53  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn53
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_3  22# happyReduction_81
happyReduction_81 _
	(HappyAbsSyn53  happy_var_2)
	_
	 =  HappyAbsSyn42
		 (AbsGrammar.ELis happy_var_2
	)
happyReduction_81 _ _ _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_3  22# happyReduction_82
happyReduction_82 _
	(HappyAbsSyn53  happy_var_2)
	_
	 =  HappyAbsSyn42
		 (AbsGrammar.ETup happy_var_2
	)
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  22# happyReduction_83
happyReduction_83 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn42
		 (AbsGrammar.EInt happy_var_1
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_1  22# happyReduction_84
happyReduction_84 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn42
		 (AbsGrammar.ECon happy_var_1
	)
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  22# happyReduction_85
happyReduction_85 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn42
		 (AbsGrammar.EVar happy_var_1
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_0  23# happyReduction_86
happyReduction_86  =  HappyAbsSyn55
		 ([]
	)

happyReduce_87 = happySpecReduce_2  23# happyReduction_87
happyReduction_87 (HappyAbsSyn55  happy_var_2)
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn55
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_87 _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_0  24# happyReduction_88
happyReduction_88  =  HappyAbsSyn56
		 ([]
	)

happyReduce_89 = happySpecReduce_2  24# happyReduction_89
happyReduction_89 (HappyAbsSyn64  happy_var_2)
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn56
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_89 _ _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_0  25# happyReduction_90
happyReduction_90  =  HappyAbsSyn57
		 ([]
	)

happyReduce_91 = happySpecReduce_1  25# happyReduction_91
happyReduction_91 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn57
		 ((:[]) happy_var_1
	)
happyReduction_91 _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_3  25# happyReduction_92
happyReduction_92 (HappyAbsSyn57  happy_var_3)
	_
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn57
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_92 _ _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_0  26# happyReduction_93
happyReduction_93  =  HappyAbsSyn57
		 ([]
	)

happyReduce_94 = happySpecReduce_2  26# happyReduction_94
happyReduction_94 (HappyAbsSyn60  happy_var_2)
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn57
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_94 _ _  = notHappyAtAll 

happyReduce_95 = happyReduce 4# 27# happyReduction_95
happyReduction_95 ((HappyAbsSyn60  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn55  happy_var_2) `HappyStk`
	(HappyAbsSyn34  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn59
		 (AbsGrammar.TDef happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_96 = happySpecReduce_3  27# happyReduction_96
happyReduction_96 (HappyAbsSyn60  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn59
		 (AbsGrammar.TClar happy_var_1 happy_var_3
	)
happyReduction_96 _ _ _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_2  28# happyReduction_97
happyReduction_97 (HappyAbsSyn57  happy_var_2)
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn60
		 (AbsGrammar.TBnb happy_var_1 (reverse happy_var_2)
	)
happyReduction_97 _ _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_1  28# happyReduction_98
happyReduction_98 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn60
		 (AbsGrammar.TUnb happy_var_1
	)
happyReduction_98 _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_3  28# happyReduction_99
happyReduction_99 _
	(HappyAbsSyn57  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (AbsGrammar.TTup happy_var_2
	)
happyReduction_99 _ _ _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_3  28# happyReduction_100
happyReduction_100 _
	(HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (AbsGrammar.TLis happy_var_2
	)
happyReduction_100 _ _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_3  29# happyReduction_101
happyReduction_101 (HappyAbsSyn60  happy_var_3)
	_
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (AbsGrammar.TFun happy_var_1 happy_var_3
	)
happyReduction_101 _ _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_1  29# happyReduction_102
happyReduction_102 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (happy_var_1
	)
happyReduction_102 _  = notHappyAtAll 

happyReduce_103 = happyReduce 4# 30# happyReduction_103
happyReduction_103 (_ `HappyStk`
	(HappyAbsSyn56  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn60
		 (AbsGrammar.TCData (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_104 = happySpecReduce_1  30# happyReduction_104
happyReduction_104 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (happy_var_1
	)
happyReduction_104 _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_1  31# happyReduction_105
happyReduction_105 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (happy_var_1
	)
happyReduction_105 _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_3  32# happyReduction_106
happyReduction_106 (HappyAbsSyn57  happy_var_3)
	(HappyAbsSyn34  happy_var_2)
	_
	 =  HappyAbsSyn64
		 (AbsGrammar.SubType happy_var_2 (reverse happy_var_3)
	)
happyReduction_106 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	happyDoAction 34# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TV happy_dollar_dollar) -> cont 30#;
	PT _ (TI happy_dollar_dollar) -> cont 31#;
	PT _ (T_UIdent happy_dollar_dollar) -> cont 32#;
	PT _ (T_LIdent happy_dollar_dollar) -> cont 33#;
	_ -> happyError' (tk:tks)
	}

happyError_ 34# tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pListTopDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> case x of {HappyAbsSyn36 z -> happyReturn z; _other -> notHappyAtAll })

pProgram tks = happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> case x of {HappyAbsSyn37 z -> happyReturn z; _other -> notHappyAtAll })

pTopDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse 2# tks) (\x -> case x of {HappyAbsSyn38 z -> happyReturn z; _other -> notHappyAtAll })

pVDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse 3# tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pListIdent tks = happySomeParser where
  happySomeParser = happyThen (happyParse 4# tks) (\x -> case x of {HappyAbsSyn40 z -> happyReturn z; _other -> notHappyAtAll })

pListMCase tks = happySomeParser where
  happySomeParser = happyThen (happyParse 5# tks) (\x -> case x of {HappyAbsSyn41 z -> happyReturn z; _other -> notHappyAtAll })

pExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse 6# tks) (\x -> case x of {HappyAbsSyn42 z -> happyReturn z; _other -> notHappyAtAll })

pMCase tks = happySomeParser where
  happySomeParser = happyThen (happyParse 7# tks) (\x -> case x of {HappyAbsSyn43 z -> happyReturn z; _other -> notHappyAtAll })

pListBind tks = happySomeParser where
  happySomeParser = happyThen (happyParse 8# tks) (\x -> case x of {HappyAbsSyn44 z -> happyReturn z; _other -> notHappyAtAll })

pBind1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 9# tks) (\x -> case x of {HappyAbsSyn45 z -> happyReturn z; _other -> notHappyAtAll })

pListBind1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 10# tks) (\x -> case x of {HappyAbsSyn44 z -> happyReturn z; _other -> notHappyAtAll })

pBind tks = happySomeParser where
  happySomeParser = happyThen (happyParse 11# tks) (\x -> case x of {HappyAbsSyn45 z -> happyReturn z; _other -> notHappyAtAll })

pListVDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse 12# tks) (\x -> case x of {HappyAbsSyn48 z -> happyReturn z; _other -> notHappyAtAll })

pExp2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 13# tks) (\x -> case x of {HappyAbsSyn42 z -> happyReturn z; _other -> notHappyAtAll })

pExp1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 14# tks) (\x -> case x of {HappyAbsSyn42 z -> happyReturn z; _other -> notHappyAtAll })

pExp3 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 15# tks) (\x -> case x of {HappyAbsSyn42 z -> happyReturn z; _other -> notHappyAtAll })

pExp4 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 16# tks) (\x -> case x of {HappyAbsSyn42 z -> happyReturn z; _other -> notHappyAtAll })

pListExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse 17# tks) (\x -> case x of {HappyAbsSyn53 z -> happyReturn z; _other -> notHappyAtAll })

pExp5 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 18# tks) (\x -> case x of {HappyAbsSyn42 z -> happyReturn z; _other -> notHappyAtAll })

pListLIdent tks = happySomeParser where
  happySomeParser = happyThen (happyParse 19# tks) (\x -> case x of {HappyAbsSyn55 z -> happyReturn z; _other -> notHappyAtAll })

pListSubType tks = happySomeParser where
  happySomeParser = happyThen (happyParse 20# tks) (\x -> case x of {HappyAbsSyn56 z -> happyReturn z; _other -> notHappyAtAll })

pListType tks = happySomeParser where
  happySomeParser = happyThen (happyParse 21# tks) (\x -> case x of {HappyAbsSyn57 z -> happyReturn z; _other -> notHappyAtAll })

pListType3 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 22# tks) (\x -> case x of {HappyAbsSyn57 z -> happyReturn z; _other -> notHappyAtAll })

pTDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse 23# tks) (\x -> case x of {HappyAbsSyn59 z -> happyReturn z; _other -> notHappyAtAll })

pType3 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 24# tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pType2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 25# tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pType1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 26# tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pType tks = happySomeParser where
  happySomeParser = happyThen (happyParse 27# tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pSubType tks = happySomeParser where
  happySomeParser = happyThen (happyParse 28# tks) (\x -> case x of {HappyAbsSyn64 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 10 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 46 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (HappyErrorToken (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
