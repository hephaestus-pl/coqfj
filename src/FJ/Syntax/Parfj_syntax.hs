{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module FJ.Syntax.Parfj_syntax where
import FJ.Syntax.Absfj_syntax
import FJ.Syntax.Lexfj_syntax
import FJ.Syntax.ErrM
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts

-- parser produced by Happy Version 1.19.0

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn24 :: (Id) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (Id)
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (Program) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (Program)
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (ClassDecl) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (ClassDecl)
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: (FieldDecl) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> (FieldDecl)
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (Constructor) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (Constructor)
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (FieldParam) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (FieldParam)
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: (FormalArg) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> (FormalArg)
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (Argument) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (Argument)
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (Assignment) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> (Assignment)
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (MethodDecl) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (MethodDecl)
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: (Term) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> (Term)
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (Access) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (Access)
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: (Exp) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> (Exp)
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (ClassName) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (ClassName)
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: ([ClassDecl]) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> ([ClassDecl])
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: ([FieldDecl]) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> ([FieldDecl])
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: ([MethodDecl]) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> ([MethodDecl])
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: ([FieldParam]) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> ([FieldParam])
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: ([FormalArg]) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> ([FormalArg])
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: ([Argument]) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> ([Argument])
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: ([Assignment]) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> ([Assignment])
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: ([Term]) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> ([Term])
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\xce\x00\x7f\x00\xcd\x00\x7f\x00\x7f\x00\xcd\x00\xcc\x00\x7f\x00\x3f\x00\x3f\x00\x80\x00\x75\x00\x00\x00\x00\x00\x00\x00\x75\x00\x75\x00\xca\x00\x00\x00\x3f\x00\xca\x00\x00\x00\x00\x00\x67\x00\xcf\x00\x00\x00\xc9\x00\x75\x00\xc7\x00\x00\x00\xf6\xff\x00\x00\xc8\x00\xc6\x00\x00\x00\xc5\x00\xc4\x00\xc3\x00\x00\x00\xc2\x00\xc1\x00\xc0\x00\x6b\x00\x6b\x00\x72\x00\xc0\x00\xc0\x00\x00\x00\x06\x00\xc0\x00\xc0\x00\xbf\x00\xbe\x00\xbd\x00\xbc\x00\xbc\x00\xbc\x00\xbb\x00\xba\x00\xba\x00\xb9\x00\xb8\x00\xb4\x00\x8a\x00\x81\x00\x00\x00\x00\x00\xb1\x00\xb6\x00\x75\x00\xb0\x00\xb7\x00\xae\x00\x00\x00\x00\x00\x00\x00\x75\x00\x00\x00\x75\x00\xae\x00\x00\x00\xb5\x00\xb3\x00\x3f\x00\x00\x00\x3f\x00\x3f\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x75\x00\xac\x00\xaf\x00\x00\x00\x75\x00\xab\x00\xa9\x00\xa4\x00\xad\x00\x3f\x00\xa8\x00\xaa\x00\x00\x00\xa7\x00\xa2\x00\xa3\x00\xa1\x00\x00\x00\x75\x00\xa6\x00\x00\x00\xa0\x00\x00\x00\x3f\x00\x8b\x00\xa5\x00\x00\x00\x78\x00\x9e\x00\x9f\x00\x9d\x00\x9c\x00\x00\x00\x00\x00\x00\x00\x36\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x54\x00\x9b\x00\x61\x00\x8d\x00\x56\x00\x52\x00\x8c\x00\x9a\x00\x51\x00\x6c\x00\x69\x00\x99\x00\x53\x00\x95\x00\x8f\x00\x70\x00\x3a\x00\x26\x00\x1b\x00\x86\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x98\x00\x00\x00\x91\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x97\x00\x00\x00\x00\x00\x00\x00\x96\x00\x00\x00\x50\x00\x39\x00\x93\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x94\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x92\x00\x00\x00\x90\x00\x00\x00\x77\x00\x00\x00\x00\x00\x00\x00\x00\x00\x31\x00\x8e\x00\x00\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x25\x00\x1a\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x65\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x17\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x45\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5c\x00\x02\x00\x00\x00\x18\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf2\xff\x00\x00\xfd\xff\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xd6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd6\xff\xd4\xff\xd2\xff\xd0\xff\xcd\xff\xca\xff\xc7\xff\xc5\xff\x00\x00\xea\xff\xe0\xff\xc4\xff\x00\x00\xdd\xff\x00\x00\x00\x00\x00\x00\xdc\xff\x00\x00\xe3\xff\xc9\xff\x00\x00\xd7\xff\xcc\xff\x00\x00\x00\x00\xd8\xff\xcf\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdb\xff\x00\x00\xdb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd5\xff\xe9\xff\x00\x00\x00\x00\xd0\xff\x00\x00\x00\x00\x00\x00\xd3\xff\xd1\xff\xe5\xff\xd0\xff\xe4\xff\xcd\xff\xca\xff\xc6\xff\x00\x00\x00\x00\xc5\xff\xc3\xff\x00\x00\xc5\xff\xc8\xff\xcb\xff\xce\xff\xdf\xff\xcd\xff\x00\x00\x00\x00\xe7\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc5\xff\x00\x00\xda\xff\xd9\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd4\xff\x00\x00\x00\x00\xe2\xff\x00\x00\xde\xff\x00\x00\xca\xff\xd7\xff\xd2\xff\x00\x00\x00\x00\xdb\xff\x00\x00\x00\x00\xe8\xff\xc7\xff\xe1\xff\x00\x00\xe6\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x00\x00\x00\x00\x0d\x00\x00\x00\x08\x00\x14\x00\x00\x00\x12\x00\x07\x00\x04\x00\x0a\x00\x0b\x00\x0c\x00\x0a\x00\x0b\x00\x0c\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x13\x00\x15\x00\x00\x00\x12\x00\x15\x00\x00\x00\x00\x00\x15\x00\x00\x00\x0a\x00\x0b\x00\x0c\x00\x07\x00\x07\x00\x06\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x10\x00\x15\x00\x0d\x00\x06\x00\x06\x00\x13\x00\x13\x00\x12\x00\x00\x00\x00\x00\x0d\x00\x0d\x00\x0d\x00\x05\x00\x05\x00\x12\x00\x12\x00\x00\x00\x00\x00\x00\x00\x03\x00\x0d\x00\x0d\x00\x05\x00\x01\x00\x11\x00\x11\x00\x0d\x00\x09\x00\x0f\x00\x0d\x00\x0d\x00\x0d\x00\x0a\x00\x00\x00\x11\x00\x0d\x00\x03\x00\x04\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x01\x00\x00\x00\x0d\x00\x06\x00\x09\x00\x09\x00\x05\x00\x00\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x00\x00\x0e\x00\x0d\x00\x03\x00\x00\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x03\x00\x04\x00\x00\x00\x00\x00\x0d\x00\x0a\x00\x0b\x00\x0c\x00\x07\x00\x0a\x00\x0b\x00\x0c\x00\x0a\x00\x0b\x00\x0c\x00\x02\x00\x08\x00\x10\x00\x07\x00\x12\x00\x00\x00\x07\x00\x10\x00\x01\x00\x01\x00\x0c\x00\x12\x00\x10\x00\x07\x00\x0f\x00\x10\x00\x08\x00\x0a\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x04\x00\x00\x00\x07\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x08\x00\x14\x00\x10\x00\x12\x00\x02\x00\x0f\x00\xff\xff\x02\x00\x05\x00\x08\x00\x0e\x00\x05\x00\x0c\x00\x01\x00\x01\x00\x05\x00\x02\x00\x02\x00\x0b\x00\x0f\x00\x0c\x00\x04\x00\x02\x00\x0e\x00\x02\x00\x06\x00\x01\x00\x10\x00\x02\x00\x01\x00\x0e\x00\x01\x00\x0e\x00\x09\x00\x05\x00\x01\x00\xff\xff\x10\x00\xff\xff\x10\x00\x04\x00\xff\xff\xff\xff\x10\x00\x03\x00\xff\xff\xff\xff\x03\x00\x10\x00\x12\x00\x03\x00\x12\x00\xff\xff\x12\x00\x10\x00\x12\x00\x10\x00\x12\x00\x04\x00\x10\x00\x12\x00\x08\x00\x10\x00\x12\x00\x0d\x00\x10\x00\x12\x00\xff\xff\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x17\x00\x20\x00\x37\x00\x17\x00\x51\x00\x7f\x00\x17\x00\xff\xff\x21\x00\x4a\x00\x18\x00\x19\x00\x1a\x00\x18\x00\x19\x00\x1a\x00\x18\x00\x19\x00\x1a\x00\x17\x00\x78\x00\x69\x00\x23\x00\xff\xff\x66\x00\x20\x00\x20\x00\x55\x00\x23\x00\x18\x00\x19\x00\x1a\x00\x21\x00\x21\x00\x24\x00\x61\x00\x23\x00\x23\x00\x23\x00\x77\x00\x1b\x00\x25\x00\x24\x00\x24\x00\x58\x00\x22\x00\x64\x00\x23\x00\x23\x00\x25\x00\x25\x00\x53\x00\x28\x00\x28\x00\x59\x00\x26\x00\x23\x00\x23\x00\x23\x00\x4a\x00\x29\x00\x29\x00\x28\x00\x1d\x00\x5a\x00\x5e\x00\x37\x00\x4b\x00\x81\x00\x3d\x00\x29\x00\x34\x00\x1e\x00\x75\x00\x2a\x00\x1f\x00\x4a\x00\x76\x00\x17\x00\x23\x00\x23\x00\x23\x00\x23\x00\x6e\x00\x40\x00\x23\x00\x3d\x00\x38\x00\x4b\x00\x33\x00\x39\x00\x17\x00\x34\x00\x34\x00\x25\x00\x2e\x00\x23\x00\x41\x00\x29\x00\x3c\x00\x17\x00\x79\x00\x19\x00\x1a\x00\x17\x00\x55\x00\xdb\xff\x17\x00\x6b\x00\x3d\x00\x67\x00\x19\x00\x1a\x00\x28\x00\x30\x00\x31\x00\x1a\x00\x32\x00\x19\x00\x1a\x00\x42\x00\x40\x00\x17\x00\x28\x00\xff\xff\x5b\x00\x28\x00\x2b\x00\x1d\x00\x1d\x00\x43\x00\xff\xff\x17\x00\x28\x00\x7d\x00\x17\x00\x40\x00\x1e\x00\x1e\x00\x20\x00\x3a\x00\x5d\x00\x17\x00\x44\x00\x3b\x00\x45\x00\x37\x00\x48\x00\x42\x00\x4c\x00\x4e\x00\x52\x00\x51\x00\x1f\x00\x17\x00\xff\xff\x3e\x00\x2c\x00\x00\x00\x7c\x00\x7e\x00\x35\x00\x2d\x00\x7b\x00\x2f\x00\x47\x00\x75\x00\x71\x00\x73\x00\x69\x00\x74\x00\x7f\x00\x70\x00\xda\xff\x6b\x00\x72\x00\x63\x00\x64\x00\x66\x00\x17\x00\x57\x00\x58\x00\x6d\x00\x5d\x00\x6e\x00\x61\x00\x60\x00\x47\x00\x00\x00\x17\x00\x00\x00\x17\x00\x48\x00\x00\x00\x00\x00\x17\x00\x4e\x00\x00\x00\x00\x00\x50\x00\x17\x00\xff\xff\x51\x00\xff\xff\x00\x00\xff\xff\x17\x00\xff\xff\x17\x00\xff\xff\x4a\x00\x17\x00\xff\xff\x40\x00\x17\x00\xff\xff\x37\x00\x17\x00\xff\xff\x00\x00\x17\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (21, 60) [
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
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
	(60 , happyReduce_60)
	]

happy_n_terms = 19 :: Int
happy_n_nonterms = 22 :: Int

happyReduce_21 = happySpecReduce_1  0# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_Id happy_var_1)) -> 
	happyIn24
		 (Id (happy_var_1)
	)}

happyReduce_22 = happySpecReduce_2  1# happyReduction_22
happyReduction_22 happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut36 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 (CProgram (reverse happy_var_1) happy_var_2
	)}}

happyReduce_23 = happyReduce 9# 2# happyReduction_23
happyReduction_23 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_2 of { happy_var_2 -> 
	case happyOut37 happy_x_4 of { happy_var_4 -> 
	case happyOut39 happy_x_6 of { happy_var_6 -> 
	case happyOut28 happy_x_7 of { happy_var_7 -> 
	case happyOut40 happy_x_8 of { happy_var_8 -> 
	happyIn26
		 (CDecl happy_var_2 happy_var_4 (reverse happy_var_6) happy_var_7 (reverse happy_var_8)
	) `HappyStk` happyRest}}}}}

happyReduce_24 = happySpecReduce_3  3# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_2 of { happy_var_2 -> 
	happyIn27
		 (FDecl happy_var_1 happy_var_2
	)}}

happyReduce_25 = happyReduce 12# 4# happyReduction_25
happyReduction_25 (happy_x_12 `HappyStk`
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
	 = case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut41 happy_x_3 of { happy_var_3 -> 
	case happyOut43 happy_x_8 of { happy_var_8 -> 
	case happyOut44 happy_x_11 of { happy_var_11 -> 
	happyIn28
		 (KDecl happy_var_1 happy_var_3 happy_var_8 (reverse happy_var_11)
	) `HappyStk` happyRest}}}}

happyReduce_26 = happySpecReduce_2  5# happyReduction_26
happyReduction_26 happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_2 of { happy_var_2 -> 
	happyIn29
		 (Field happy_var_1 happy_var_2
	)}}

happyReduce_27 = happySpecReduce_2  6# happyReduction_27
happyReduction_27 happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_2 of { happy_var_2 -> 
	happyIn30
		 (FArg happy_var_1 happy_var_2
	)}}

happyReduce_28 = happySpecReduce_1  7# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 (Arg happy_var_1
	)}

happyReduce_29 = happyReduce 6# 8# happyReduction_29
happyReduction_29 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_3 of { happy_var_3 -> 
	case happyOut24 happy_x_5 of { happy_var_5 -> 
	happyIn32
		 (Assgnmt happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_30 = happyReduce 10# 9# happyReduction_30
happyReduction_30 (happy_x_10 `HappyStk`
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
	 = case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_2 of { happy_var_2 -> 
	case happyOut42 happy_x_4 of { happy_var_4 -> 
	case happyOut34 happy_x_8 of { happy_var_8 -> 
	happyIn33
		 (MDecl happy_var_1 happy_var_2 happy_var_4 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_31 = happySpecReduce_1  10# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 (TermVar happy_var_1
	)}

happyReduce_32 = happySpecReduce_3  10# happyReduction_32
happyReduction_32 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	happyIn34
		 (TermFieldAccess happy_var_1 happy_var_3
	)}}

happyReduce_33 = happyReduce 6# 10# happyReduction_33
happyReduction_33 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	case happyOut45 happy_x_5 of { happy_var_5 -> 
	happyIn34
		 (TermMethodInvoc happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_34 = happySpecReduce_1  10# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 (TermExp happy_var_1
	)}

happyReduce_35 = happySpecReduce_1  11# happyReduction_35
happyReduction_35 happy_x_1
	 =  happyIn35
		 (ThisAccess
	)

happyReduce_36 = happySpecReduce_1  11# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (TermAcces happy_var_1
	)}

happyReduce_37 = happyReduce 4# 12# happyReduction_37
happyReduction_37 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_2 of { happy_var_2 -> 
	case happyOut34 happy_x_4 of { happy_var_4 -> 
	happyIn36
		 (CastExp happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_38 = happyReduce 5# 12# happyReduction_38
happyReduction_38 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_2 of { happy_var_2 -> 
	case happyOut45 happy_x_4 of { happy_var_4 -> 
	happyIn36
		 (NewExp happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_39 = happySpecReduce_1  13# happyReduction_39
happyReduction_39 happy_x_1
	 =  happyIn37
		 (ClassObject
	)

happyReduce_40 = happySpecReduce_1  13# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (ClassId happy_var_1
	)}

happyReduce_41 = happySpecReduce_0  14# happyReduction_41
happyReduction_41  =  happyIn38
		 ([]
	)

happyReduce_42 = happySpecReduce_2  14# happyReduction_42
happyReduction_42 happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_2 of { happy_var_2 -> 
	happyIn38
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_43 = happySpecReduce_0  15# happyReduction_43
happyReduction_43  =  happyIn39
		 ([]
	)

happyReduce_44 = happySpecReduce_2  15# happyReduction_44
happyReduction_44 happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_2 of { happy_var_2 -> 
	happyIn39
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_45 = happySpecReduce_0  16# happyReduction_45
happyReduction_45  =  happyIn40
		 ([]
	)

happyReduce_46 = happySpecReduce_2  16# happyReduction_46
happyReduction_46 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_2 of { happy_var_2 -> 
	happyIn40
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_47 = happySpecReduce_0  17# happyReduction_47
happyReduction_47  =  happyIn41
		 ([]
	)

happyReduce_48 = happySpecReduce_1  17# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 ((:[]) happy_var_1
	)}

happyReduce_49 = happySpecReduce_3  17# happyReduction_49
happyReduction_49 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	case happyOut41 happy_x_3 of { happy_var_3 -> 
	happyIn41
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_50 = happySpecReduce_0  18# happyReduction_50
happyReduction_50  =  happyIn42
		 ([]
	)

happyReduce_51 = happySpecReduce_1  18# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 ((:[]) happy_var_1
	)}

happyReduce_52 = happySpecReduce_3  18# happyReduction_52
happyReduction_52 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_53 = happySpecReduce_0  19# happyReduction_53
happyReduction_53  =  happyIn43
		 ([]
	)

happyReduce_54 = happySpecReduce_1  19# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 ((:[]) happy_var_1
	)}

happyReduce_55 = happySpecReduce_3  19# happyReduction_55
happyReduction_55 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_56 = happySpecReduce_0  20# happyReduction_56
happyReduction_56  =  happyIn44
		 ([]
	)

happyReduce_57 = happySpecReduce_2  20# happyReduction_57
happyReduction_57 happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_2 of { happy_var_2 -> 
	happyIn44
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_58 = happySpecReduce_0  21# happyReduction_58
happyReduction_58  =  happyIn45
		 ([]
	)

happyReduce_59 = happySpecReduce_1  21# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 ((:[]) happy_var_1
	)}

happyReduce_60 = happySpecReduce_3  21# happyReduction_60
happyReduction_60 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 ((:) happy_var_1 happy_var_3
	)}}

happyNewToken action sts stk [] =
	happyDoAction 18# notHappyAtAll action sts stk []

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
	PT _ (T_Id happy_dollar_dollar) -> cont 16#;
	_ -> cont 17#;
	_ -> happyError' (tk:tks)
	}

happyError_ 18# tk tks = happyError' tks
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

pProgram tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut25 x))

pClassDecl tks = happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut26 x))

pFieldDecl tks = happySomeParser where
  happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut27 x))

pConstructor tks = happySomeParser where
  happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut28 x))

pFieldParam tks = happySomeParser where
  happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut29 x))

pFormalArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut30 x))

pArgument tks = happySomeParser where
  happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut31 x))

pAssignment tks = happySomeParser where
  happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut32 x))

pMethodDecl tks = happySomeParser where
  happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut33 x))

pTerm tks = happySomeParser where
  happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut34 x))

pAccess tks = happySomeParser where
  happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (happyOut35 x))

pExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (happyOut36 x))

pClassName tks = happySomeParser where
  happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (happyOut37 x))

pListClassDecl tks = happySomeParser where
  happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (happyOut38 x))

pListFieldDecl tks = happySomeParser where
  happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (happyOut39 x))

pListMethodDecl tks = happySomeParser where
  happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (happyOut40 x))

pListFieldParam tks = happySomeParser where
  happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (happyOut41 x))

pListFormalArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (happyOut42 x))

pListArgument tks = happySomeParser where
  happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (happyOut43 x))

pListAssignment tks = happySomeParser where
  happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (happyOut44 x))

pListTerm tks = happySomeParser where
  happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (happyOut45 x))

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
