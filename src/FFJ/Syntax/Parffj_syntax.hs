{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module FFJ.Syntax.Parffj_syntax where
import FFJ.Syntax.Absffj_syntax
import FFJ.Syntax.Lexffj_syntax
import FFJ.Syntax.ErrM
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts

-- parser produced by Happy Version 1.19.0

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn28 :: (Id) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (Id)
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (CDList) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (CDList)
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: (CDef) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> (CDef)
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (CD) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (CD)
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (CR) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> (CR)
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (FD) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (FD)
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: (KD) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> (KD)
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (KR) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (KR)
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: (Field) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> (Field)
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (FormalArg) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (FormalArg)
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: (Arg) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> (Arg)
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: (Assignment) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> (Assignment)
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (MD) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (MD)
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: (MR) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> (MR)
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: (Type) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> (Type)
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (Term) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> (Term)
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: (Exp) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> (Exp)
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: ([CDef]) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> ([CDef])
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: ([FD]) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> ([FD])
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: ([MD]) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> ([MD])
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: ([MR]) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> ([MR])
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: ([Field]) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> ([Field])
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: ([FormalArg]) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> ([FormalArg])
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyIn51 :: ([Arg]) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> ([Arg])
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
happyIn52 :: ([Assignment]) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> ([Assignment])
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
happyIn53 :: ([Term]) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> ([Term])
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x99\x00\x1c\x01\x16\x01\x8d\x00\x11\x01\x14\x01\x8d\x00\x8d\x00\x0f\x01\x10\x01\x8d\x00\x0e\x01\x8d\x00\x4c\x00\x86\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8d\x00\x8d\x00\x0c\x01\x00\x00\x4c\x00\x0c\x01\x00\x00\x00\x00\xb2\x00\x00\x00\x09\x01\x8d\x00\x0a\x01\xf9\xff\x00\x00\x0b\x01\x07\x01\x00\x00\x08\x01\x06\x01\x05\x01\x00\x00\x04\x01\x03\x01\x02\x01\x90\x00\x83\x00\x83\x00\x85\x00\x02\x01\x22\x00\x02\x01\x02\x01\x01\x01\x00\x01\xff\x00\xfe\x00\xfd\x00\xfb\x00\xfb\x00\xfb\x00\xfb\x00\xfa\x00\xfc\x00\xf9\x00\xf9\x00\xf8\x00\xf4\x00\xf7\x00\xee\x00\xec\x00\xe4\x00\x00\x00\x00\x00\xe4\x00\x8e\x00\x00\x00\x00\x00\xf2\x00\xea\x00\xed\x00\x8d\x00\xf6\x00\xe8\x00\xf5\x00\xe3\x00\xe3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8d\x00\x00\x00\x8d\x00\xe3\x00\x00\x00\xf3\x00\xf1\x00\x4c\x00\x00\x00\x4c\x00\x4c\x00\x00\x00\x00\x00\x00\x00\xf0\x00\xef\x00\x8d\x00\xe9\x00\x8d\x00\xeb\x00\x00\x00\xde\x00\x8d\x00\xdc\x00\x00\x00\xda\x00\xe7\x00\xd9\x00\xe6\x00\x8d\x00\x4c\x00\xe5\x00\xe0\x00\x00\x00\xe1\x00\xdd\x00\xd6\x00\xd8\x00\xd5\x00\xce\x00\x94\x00\x00\x00\x8d\x00\x00\x00\xd3\x00\xd7\x00\x00\x00\xd4\x00\xd0\x00\x00\x00\xcd\x00\x4c\x00\xd2\x00\xcc\x00\x8d\x00\xd1\x00\x00\x00\x8c\x00\xfa\xff\xcf\x00\xc9\x00\xaf\x00\x4c\x00\xad\x00\xc8\x00\xca\x00\xcb\x00\x00\x00\x00\x00\x00\x00\xc6\x00\x00\x00\xc5\x00\x00\x00\x00\x00\x9f\x00\x00\x00\x9e\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x4b\x00\xa8\x00\xc7\x00\xc3\x00\x84\x00\x09\x00\xc2\x00\x7a\x00\x78\x00\x21\x00\xc4\x00\x77\x00\xc0\x00\x7e\x00\x5a\x00\xb4\x00\xbd\x00\xb6\x00\xb3\x00\xb1\x00\x46\x00\x29\x00\x1d\x00\xaa\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7d\x00\xc1\x00\xb5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbf\x00\x00\x00\x00\x00\x00\x00\xbe\x00\x00\x00\xb0\x00\x6d\x00\x71\x00\xa5\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbc\x00\x00\x00\xbb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xba\x00\x00\x00\x00\x00\x00\x00\xb9\x00\x00\x00\x00\x00\x00\x00\xb8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5f\x00\x00\x00\x00\x00\x00\x00\xb7\x00\x00\x00\x3c\x00\x00\x00\xa3\x00\x00\x00\xa2\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x25\x00\x18\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x58\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x00\x00\x7c\x00\x00\x00\x00\x00\x65\x00\x00\x00\x1f\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6e\x00\x47\x00\x66\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x04\x00\x49\x00\x00\x00\x2e\x00\x64\x00\x33\x00\x00\x00\x02\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xcf\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcf\xff\xcd\xff\xcb\xff\xc9\xff\xc7\xff\xc4\xff\xc1\xff\xbe\xff\xbc\xff\x00\x00\xe6\xff\xd5\xff\xbb\xff\xd2\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdb\xff\xc0\xff\x00\x00\xd6\xff\xc3\xff\x00\x00\x00\x00\xd7\xff\xc6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe4\xff\xe3\xff\x00\x00\x00\x00\xce\xff\xe5\xff\x00\x00\x00\x00\x00\x00\xc7\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcc\xff\xca\xff\xc8\xff\xdd\xff\xc7\xff\xdc\xff\xc4\xff\xc1\xff\xbd\xff\x00\x00\x00\x00\xbc\xff\xba\xff\x00\x00\xbc\xff\xbf\xff\xc2\xff\xc5\xff\xd4\xff\x00\x00\xc4\xff\x00\x00\xc7\xff\x00\x00\xe0\xff\x00\x00\x00\x00\x00\x00\xcd\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc4\xff\xbc\xff\x00\x00\xd1\xff\xd0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcd\xff\x00\x00\xcb\xff\x00\x00\x00\x00\xda\xff\x00\x00\x00\x00\xd3\xff\x00\x00\x00\x00\x00\x00\xc1\xff\xc9\xff\xd6\xff\xcb\xff\x00\x00\x00\x00\x00\x00\xc1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe1\xff\xe2\xff\xbe\xff\x00\x00\xd9\xff\x00\x00\xd8\xff\xbe\xff\x00\x00\xdf\xff\x00\x00\xde\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x0f\x00\x00\x00\x0b\x00\x11\x00\x0a\x00\x14\x00\x0a\x00\x06\x00\x0f\x00\x10\x00\x0f\x00\x10\x00\x0f\x00\x10\x00\x0f\x00\x10\x00\x00\x00\x17\x00\x19\x00\x17\x00\x19\x00\x00\x00\x19\x00\x00\x00\x19\x00\x00\x00\x0a\x00\x00\x00\x18\x00\x00\x00\x04\x00\x0a\x00\x09\x00\x00\x00\x18\x00\x0a\x00\x09\x00\x0e\x00\x09\x00\x17\x00\x00\x00\x0e\x00\x09\x00\x0e\x00\x17\x00\x16\x00\x14\x00\x0e\x00\x08\x00\x16\x00\x00\x00\x16\x00\x00\x00\x00\x00\x0e\x00\x16\x00\x0d\x00\x13\x00\x08\x00\x00\x00\x08\x00\x15\x00\x00\x00\x13\x00\x0e\x00\x00\x00\x0e\x00\x0e\x00\x01\x00\x01\x00\x08\x00\x15\x00\x00\x00\x15\x00\x0f\x00\x10\x00\x0e\x00\x0c\x00\x0a\x00\x0e\x00\x00\x00\x12\x00\x00\x00\x15\x00\x11\x00\x14\x00\x12\x00\x0f\x00\x10\x00\x02\x00\x03\x00\x04\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x10\x00\x0f\x00\x10\x00\x05\x00\x06\x00\x00\x00\x00\x00\x10\x00\x0c\x00\x00\x00\x0e\x00\x05\x00\x0e\x00\x07\x00\x05\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x0e\x00\x0e\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x09\x00\x08\x00\x0c\x00\x00\x00\x0e\x00\x0e\x00\x01\x00\x0e\x00\x05\x00\x07\x00\x0e\x00\x0e\x00\x08\x00\x12\x00\x01\x00\x0a\x00\x0c\x00\x0e\x00\x07\x00\x07\x00\x12\x00\x08\x00\x14\x00\x0a\x00\x14\x00\x0c\x00\x07\x00\x0c\x00\x11\x00\x12\x00\x12\x00\x0c\x00\x08\x00\x00\x00\x00\x00\x14\x00\x0c\x00\x12\x00\x02\x00\x03\x00\x04\x00\x02\x00\x03\x00\x04\x00\x0f\x00\x0f\x00\x11\x00\x11\x00\x04\x00\x05\x00\x04\x00\x05\x00\x03\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x0b\x00\x00\x00\x18\x00\xff\xff\x10\x00\x14\x00\x13\x00\x04\x00\x12\x00\x07\x00\x03\x00\x05\x00\x02\x00\x0d\x00\x11\x00\x0b\x00\x05\x00\x02\x00\x01\x00\x01\x00\x01\x00\xff\xff\x11\x00\xff\xff\xff\xff\x11\x00\x0d\x00\x12\x00\x0e\x00\x05\x00\x12\x00\x02\x00\x10\x00\x0d\x00\x0b\x00\x02\x00\x04\x00\x10\x00\x10\x00\x02\x00\x02\x00\x02\x00\x10\x00\x12\x00\x10\x00\x02\x00\x10\x00\x06\x00\x01\x00\x01\x00\x05\x00\x02\x00\x01\x00\x12\x00\x01\x00\x01\x00\x14\x00\xff\xff\x12\x00\x09\x00\x12\x00\x01\x00\x12\x00\x08\x00\xff\xff\x04\x00\x14\x00\xff\xff\xff\xff\xff\xff\xff\xff\x03\x00\x14\x00\xff\xff\x12\x00\x03\x00\x12\x00\x14\x00\x03\x00\x14\x00\xff\xff\x12\x00\x14\x00\x12\x00\x14\x00\x12\x00\x14\x00\xff\xff\x12\x00\x14\x00\x0c\x00\x14\x00\x12\x00\x14\x00\x12\x00\x0f\x00\x0c\x00\x12\x00\x0c\x00\x12\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x1b\x00\x22\x00\x1b\x00\x22\x00\x1b\x00\x36\x00\x1b\x00\x3a\x00\x3f\x00\x5f\x00\x9f\x00\x23\x00\xff\xff\x23\x00\x40\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x22\x00\x9c\x00\x7d\x00\x96\x00\x7a\x00\x22\x00\x63\x00\x25\x00\x1e\x00\x22\x00\x23\x00\x25\x00\xa8\x00\x25\x00\x57\x00\x23\x00\x26\x00\x25\x00\xa6\x00\x3a\x00\x26\x00\x27\x00\x26\x00\x66\x00\x25\x00\x27\x00\x26\x00\x27\x00\x24\x00\x7e\x00\xff\xff\x27\x00\x2a\x00\x77\x00\x25\x00\x67\x00\x25\x00\x25\x00\x2b\x00\x28\x00\x59\x00\x94\x00\x2a\x00\x1b\x00\x2a\x00\x75\x00\x25\x00\x91\x00\x2b\x00\x25\x00\x2b\x00\x72\x00\x4a\x00\x20\x00\x2a\x00\x68\x00\x1b\x00\x6e\x00\x9a\x00\x1d\x00\x2b\x00\x58\x00\x21\x00\x37\x00\x1b\x00\x85\x00\x1b\x00\x2c\x00\x4b\x00\x95\x00\x1b\x00\x98\x00\x1d\x00\x4c\x00\x48\x00\x49\x00\x25\x00\x80\x00\x92\x00\x7b\x00\x1d\x00\x32\x00\x1d\x00\x57\x00\x93\x00\x25\x00\x25\x00\x4d\x00\x58\x00\x25\x00\x37\x00\x57\x00\x42\x00\x86\x00\x57\x00\x25\x00\x25\x00\x58\x00\x25\x00\x37\x00\x42\x00\x25\x00\x25\x00\x42\x00\x69\x00\x3b\x00\x3c\x00\x36\x00\x25\x00\x37\x00\x27\x00\x20\x00\x2b\x00\x41\x00\x2a\x00\x61\x00\x33\x00\x47\x00\x83\x00\x20\x00\x21\x00\x45\x00\x42\x00\x2a\x00\x2a\x00\x1b\x00\x47\x00\xff\xff\x21\x00\xff\xff\x45\x00\x2a\x00\x36\x00\xa0\x00\x1b\x00\x1b\x00\x3f\x00\x47\x00\x6a\x00\x6c\x00\xff\xff\x45\x00\x1b\x00\x4c\x00\x48\x00\x49\x00\x47\x00\x48\x00\x49\x00\x3a\x00\x3a\x00\xaa\x00\xa8\x00\x57\x00\xa4\x00\x57\x00\x9c\x00\x63\x00\x57\x00\x70\x00\x4e\x00\x50\x00\x52\x00\x54\x00\x55\x00\x59\x00\x5a\x00\x5c\x00\x5f\x00\x60\x00\x21\x00\x00\x00\x31\x00\x2d\x00\x2e\x00\x43\x00\x2f\x00\x3d\x00\x45\x00\xa6\x00\xa2\x00\x34\x00\x30\x00\x38\x00\xa1\x00\x9e\x00\x52\x00\x98\x00\x91\x00\x00\x00\xa5\x00\x00\x00\x00\x00\xa3\x00\x9a\x00\x1b\x00\x88\x00\x8a\x00\x1b\x00\x8c\x00\x8e\x00\x8f\x00\x90\x00\x8d\x00\x57\x00\x89\x00\x8b\x00\x7d\x00\x80\x00\x82\x00\x83\x00\x1b\x00\x85\x00\x75\x00\x74\x00\x77\x00\x79\x00\x7a\x00\x70\x00\x65\x00\x66\x00\x1b\x00\x6c\x00\x6e\x00\xff\xff\x00\x00\x1b\x00\x72\x00\x1b\x00\x52\x00\x1b\x00\x50\x00\x00\x00\x54\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x5c\x00\xff\xff\x00\x00\x1b\x00\x5e\x00\x1b\x00\xff\xff\x5f\x00\xff\xff\x00\x00\x1b\x00\xff\xff\x1b\x00\xff\xff\x1b\x00\xff\xff\x00\x00\x1b\x00\xff\xff\x36\x00\xff\xff\x1b\x00\xff\xff\x1b\x00\x3a\x00\x3f\x00\x1b\x00\x45\x00\x1b\x00\x47\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (25, 69) [
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
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
	(69 , happyReduce_69)
	]

happy_n_terms = 21 :: Int
happy_n_nonterms = 26 :: Int

happyReduce_25 = happySpecReduce_1  0# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_Id happy_var_1)) -> 
	happyIn28
		 (Id (happy_var_1)
	)}

happyReduce_26 = happySpecReduce_2  1# happyReduction_26
happyReduction_26 happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_2 of { happy_var_2 -> 
	happyIn29
		 (CDList (reverse happy_var_1) happy_var_2
	)}}

happyReduce_27 = happySpecReduce_1  2# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 (CDDecl happy_var_1
	)}

happyReduce_28 = happySpecReduce_1  2# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 (CDRef happy_var_1
	)}

happyReduce_29 = happyReduce 9# 3# happyReduction_29
happyReduction_29 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_2 of { happy_var_2 -> 
	case happyOut42 happy_x_4 of { happy_var_4 -> 
	case happyOut46 happy_x_6 of { happy_var_6 -> 
	case happyOut34 happy_x_7 of { happy_var_7 -> 
	case happyOut47 happy_x_8 of { happy_var_8 -> 
	happyIn31
		 (CDecl happy_var_2 happy_var_4 (reverse happy_var_6) happy_var_7 (reverse happy_var_8)
	) `HappyStk` happyRest}}}}}

happyReduce_30 = happyReduce 9# 4# happyReduction_30
happyReduction_30 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_3 of { happy_var_3 -> 
	case happyOut46 happy_x_5 of { happy_var_5 -> 
	case happyOut35 happy_x_6 of { happy_var_6 -> 
	case happyOut47 happy_x_7 of { happy_var_7 -> 
	case happyOut48 happy_x_8 of { happy_var_8 -> 
	happyIn32
		 (CRef happy_var_3 (reverse happy_var_5) happy_var_6 (reverse happy_var_7) (reverse happy_var_8)
	) `HappyStk` happyRest}}}}}

happyReduce_31 = happySpecReduce_3  5# happyReduction_31
happyReduction_31 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_2 of { happy_var_2 -> 
	happyIn33
		 (FDecl happy_var_1 happy_var_2
	)}}

happyReduce_32 = happyReduce 12# 6# happyReduction_32
happyReduction_32 (happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut49 happy_x_3 of { happy_var_3 -> 
	case happyOut51 happy_x_8 of { happy_var_8 -> 
	case happyOut52 happy_x_11 of { happy_var_11 -> 
	happyIn34
		 (KDecl happy_var_1 happy_var_3 happy_var_8 (reverse happy_var_11)
	) `HappyStk` happyRest}}}}

happyReduce_33 = happyReduce 13# 7# happyReduction_33
happyReduction_33 (happy_x_13 `HappyStk`
	happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_2 of { happy_var_2 -> 
	case happyOut49 happy_x_4 of { happy_var_4 -> 
	case happyOut51 happy_x_9 of { happy_var_9 -> 
	case happyOut52 happy_x_12 of { happy_var_12 -> 
	happyIn35
		 (KRef happy_var_2 happy_var_4 happy_var_9 (reverse happy_var_12)
	) `HappyStk` happyRest}}}}

happyReduce_34 = happySpecReduce_2  8# happyReduction_34
happyReduction_34 happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_2 of { happy_var_2 -> 
	happyIn36
		 (Field happy_var_1 happy_var_2
	)}}

happyReduce_35 = happySpecReduce_2  9# happyReduction_35
happyReduction_35 happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_2 of { happy_var_2 -> 
	happyIn37
		 (FormalArg happy_var_1 happy_var_2
	)}}

happyReduce_36 = happySpecReduce_1  10# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn38
		 (Arg happy_var_1
	)}

happyReduce_37 = happyReduce 6# 11# happyReduction_37
happyReduction_37 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_3 of { happy_var_3 -> 
	case happyOut28 happy_x_5 of { happy_var_5 -> 
	happyIn39
		 (Assignment happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_38 = happyReduce 10# 12# happyReduction_38
happyReduction_38 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_2 of { happy_var_2 -> 
	case happyOut50 happy_x_4 of { happy_var_4 -> 
	case happyOut43 happy_x_8 of { happy_var_8 -> 
	happyIn40
		 (MethodDecl happy_var_1 happy_var_2 happy_var_4 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_39 = happyReduce 11# 13# happyReduction_39
happyReduction_39 (happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_2 of { happy_var_2 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	case happyOut50 happy_x_5 of { happy_var_5 -> 
	case happyOut43 happy_x_9 of { happy_var_9 -> 
	happyIn41
		 (MethodRef happy_var_2 happy_var_3 happy_var_5 happy_var_9
	) `HappyStk` happyRest}}}}

happyReduce_40 = happySpecReduce_1  14# happyReduction_40
happyReduction_40 happy_x_1
	 =  happyIn42
		 (TypeObject
	)

happyReduce_41 = happySpecReduce_1  14# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (TypeId happy_var_1
	)}

happyReduce_42 = happySpecReduce_1  15# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (TermVar happy_var_1
	)}

happyReduce_43 = happySpecReduce_3  15# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (TermFieldAccess happy_var_1 happy_var_3
	)}}

happyReduce_44 = happyReduce 6# 15# happyReduction_44
happyReduction_44 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	case happyOut53 happy_x_5 of { happy_var_5 -> 
	happyIn43
		 (TermMethodInvoc happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_45 = happySpecReduce_1  15# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (TermExp happy_var_1
	)}

happyReduce_46 = happyReduce 4# 16# happyReduction_46
happyReduction_46 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut42 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	happyIn44
		 (CastExp happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_47 = happyReduce 5# 16# happyReduction_47
happyReduction_47 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_2 of { happy_var_2 -> 
	case happyOut53 happy_x_4 of { happy_var_4 -> 
	happyIn44
		 (NewExp happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_48 = happySpecReduce_0  17# happyReduction_48
happyReduction_48  =  happyIn45
		 ([]
	)

happyReduce_49 = happySpecReduce_2  17# happyReduction_49
happyReduction_49 happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn45
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_50 = happySpecReduce_0  18# happyReduction_50
happyReduction_50  =  happyIn46
		 ([]
	)

happyReduce_51 = happySpecReduce_2  18# happyReduction_51
happyReduction_51 happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_2 of { happy_var_2 -> 
	happyIn46
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_52 = happySpecReduce_0  19# happyReduction_52
happyReduction_52  =  happyIn47
		 ([]
	)

happyReduce_53 = happySpecReduce_2  19# happyReduction_53
happyReduction_53 happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_2 of { happy_var_2 -> 
	happyIn47
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_54 = happySpecReduce_0  20# happyReduction_54
happyReduction_54  =  happyIn48
		 ([]
	)

happyReduce_55 = happySpecReduce_2  20# happyReduction_55
happyReduction_55 happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	case happyOut41 happy_x_2 of { happy_var_2 -> 
	happyIn48
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_56 = happySpecReduce_0  21# happyReduction_56
happyReduction_56  =  happyIn49
		 ([]
	)

happyReduce_57 = happySpecReduce_1  21# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 ((:[]) happy_var_1
	)}

happyReduce_58 = happySpecReduce_3  21# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut49 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_59 = happySpecReduce_0  22# happyReduction_59
happyReduction_59  =  happyIn50
		 ([]
	)

happyReduce_60 = happySpecReduce_1  22# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn50
		 ((:[]) happy_var_1
	)}

happyReduce_61 = happySpecReduce_3  22# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	happyIn50
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_62 = happySpecReduce_0  23# happyReduction_62
happyReduction_62  =  happyIn51
		 ([]
	)

happyReduce_63 = happySpecReduce_1  23# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	happyIn51
		 ((:[]) happy_var_1
	)}

happyReduce_64 = happySpecReduce_3  23# happyReduction_64
happyReduction_64 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut51 happy_x_3 of { happy_var_3 -> 
	happyIn51
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_65 = happySpecReduce_0  24# happyReduction_65
happyReduction_65  =  happyIn52
		 ([]
	)

happyReduce_66 = happySpecReduce_2  24# happyReduction_66
happyReduction_66 happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_2 of { happy_var_2 -> 
	happyIn52
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_67 = happySpecReduce_0  25# happyReduction_67
happyReduction_67  =  happyIn53
		 ([]
	)

happyReduce_68 = happySpecReduce_1  25# happyReduction_68
happyReduction_68 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn53
		 ((:[]) happy_var_1
	)}

happyReduce_69 = happySpecReduce_3  25# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 ((:) happy_var_1 happy_var_3
	)}}

happyNewToken action sts stk [] =
	happyDoAction 20# notHappyAtAll action sts stk []

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
	PT _ (T_Id happy_dollar_dollar) -> cont 18#;
	_ -> cont 19#;
	_ -> happyError' (tk:tks)
	}

happyError_ 20# tk tks = happyError' tks
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

pCDList tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut29 x))

pCDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut30 x))

pCD tks = happySomeParser where
  happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut31 x))

pCR tks = happySomeParser where
  happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut32 x))

pFD tks = happySomeParser where
  happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut33 x))

pKD tks = happySomeParser where
  happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut34 x))

pKR tks = happySomeParser where
  happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut35 x))

pField tks = happySomeParser where
  happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut36 x))

pFormalArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut37 x))

pArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut38 x))

pAssignment tks = happySomeParser where
  happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (happyOut39 x))

pMD tks = happySomeParser where
  happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (happyOut40 x))

pMR tks = happySomeParser where
  happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (happyOut41 x))

pType tks = happySomeParser where
  happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (happyOut42 x))

pTerm tks = happySomeParser where
  happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (happyOut43 x))

pExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (happyOut44 x))

pListCDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (happyOut45 x))

pListFD tks = happySomeParser where
  happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (happyOut46 x))

pListMD tks = happySomeParser where
  happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (happyOut47 x))

pListMR tks = happySomeParser where
  happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (happyOut48 x))

pListField tks = happySomeParser where
  happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (happyOut49 x))

pListFormalArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (happyOut50 x))

pListArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (happyOut51 x))

pListAssignment tks = happySomeParser where
  happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (happyOut52 x))

pListTerm tks = happySomeParser where
  happySomeParser = happyThen (happyParse 24# tks) (\x -> happyReturn (happyOut53 x))

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
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 8 "<command-line>" 2
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}





#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 45 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 66 "templates/GenericTemplate.hs" #-}

{-# LINE 76 "templates/GenericTemplate.hs" #-}

{-# LINE 85 "templates/GenericTemplate.hs" #-}

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
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

				     (happyReduceArr Happy_Data_Array.! rule) i tk st
				     where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
		n		  -> {- nothing -}


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

{-# LINE 169 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

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
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

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
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

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
