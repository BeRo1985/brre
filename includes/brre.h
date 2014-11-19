/**********************************************************
** BRRE Regular Expression Library                        *
***********************************************************
**
** This file is part of the BRRE Regular Expression Library.
** Copyright (C) 2011-2012 by Benjamin Rosseaux
**
** See the file COPYING.BRRE, included in this distribution,
** for details about the copyright.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
**
*/
#ifndef BRRE_H
#define BRRE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#ifdef __i386__
#define brreCALLCONV __cdecl
#else
#define brreCALLCONV
#endif

#define brresuDONOTKNOW -1
#define brresuNOUTF8 0
#define brresuPOSSIBLEUTF8 1
#define brresuISUTF8 2

#define brrefDELIMITERS (1 << 0)
#define brrefBACKTRACKING (1 << 1)
#define brrefFREESPACING (1 << 2)
#define brrefIGNORECASE (1 << 3)
#define brrefSINGLELINE (1 << 4)
#define brrefMULTILINE (1 << 5)
#define brrefLATIN1 (1 << 6)
#define brrefUTF8 (1 << 7)
#define brrefUTF8CODEUNITS (1 << 8)
#define brrefNAMED (1 << 9)
#define brrefCONTINUATION (1 << 10)
#define brrefAUTO (1 << 11)
#define brrefWHOLEONLY (1 << 12)
#define brrefCOMBINEOPTIMIZATION (1 << 13)
#define brrefUNGREEDY (1 << 14)
#define brrefLONGEST (1 << 15)
#define brrefNFA (1 << 16)
#define brrefDFA (1 << 17)
#define brrefNOBACKTRACKING (1 << 18)
#define brrefPREFILTER (1 << 19)
#define brrefPREFILTEROPT (1 << 20)
#define brrefAUTOCALLOUT (1 << 21)
#define brrefNOTEMPTY (1 << 22)
#define brrefRANGE (1 << 23)

#pragma pack(push)
#pragma pack(1)

typedef struct
{
    uint32_t Version;
    void* Instance;
    int32_t CalloutNumber;
    void* CalloutData;
    char* Subject;
    int32_t SubjectUnitsLength;
    int32_t SubjectPointsLength;
    void* Captures;
    int32_t CountCaptures;
    int32_t CodeUnit;
    int32_t CodePoint;
} TBRRECalloutBlock;

typedef TBRRECalloutBlock* PBRRECalloutBlock;

typedef void* TBRRERegExp;

typedef void (brreCALLCONV *TBRRECalloutFunc)(PBRRECalloutBlock*);

#pragma pack(pop)

int64_t brreCALLCONV BRREGetVersion();
char* brreCALLCONV BRREGetVersionString();
int32_t brreCALLCONV BRRECreate(TBRRERegExp Instance, char* RegExp, int32_t RegExpLen, uint32_t Flags, char** Error);
int32_t brreCALLCONV BRRESetMaxMatchLimit(TBRRERegExp Instance, int32_t MaxMatchLimit);
int32_t brreCALLCONV BRRESetMaxMatchLimitRecursion(TBRRERegExp Instance, int32_t MaxMatchLimitRecursion);
int32_t brreCALLCONV BRRESetMaxMatchLimitStackRecursion(TBRRERegExp Instance, int32_t MaxMatchLimitStackRecursion);
int32_t brreCALLCONV BRRESetCalloutFunc(TBRRERegExp Instance, void* CalloutFunc);
int32_t brreCALLCONV BRRESetCalloutData(TBRRERegExp Instance, void* CalloutData);
int32_t brreCALLCONV BRREGetCountCaptures(TBRRERegExp Instance);
int32_t brreCALLCONV BRREGetNamedGroupIndex(TBRRERegExp Instance, char* GroupName);
int32_t brreCALLCONV BRREPrefilterExpression(TBRRERegExp Instance, char** Expression, char** Error);
int32_t brreCALLCONV BRREPrefilterShortExpression(TBRRERegExp Instance, char** ShortExpression, char** Error);
int32_t brreCALLCONV BRREPrefilterSQLBooleanFullTextExpression(TBRRERegExp Instance, char** SQLBooleanFullTextExpression,char** Error);
int32_t brreCALLCONV BRREPrefilterSQLExpression(TBRRERegExp Instance, char* Field, char** SQLExpression, char** Error);
int32_t brreCALLCONV BRREGetRange(TBRRERegExp Instance, char** RangeLow, char** RangeHigh, char** Error);
int32_t brreCALLCONV BRREMatch(TBRRERegExp Instance, char* Input, int32_t InputIsUTF8, int32_t InputCodePointLen, int32_t InputCodeUnitLen, void** Captures, int32_t MaxCaptures, int32_t StartCodePoint, int32_t StartCodeUnit, int32_t* LastIndexCodePoint, int32_t* LastIndexCodeUnit, char** Error);
int32_t brreCALLCONV BRREMatchAll(TBRRERegExp Instance, char* Input, int32_t InputIsUTF8, int32_t InputCodePointLen, int32_t InputCodeUnitLen, void** Matches, int32_t StartCodePoint, int32_t StartCodeUnit, int32_t Limit, int32_t* LastIndexCodePoint, int32_t* LastIndexCodeUnit, char** Error);
int32_t brreCALLCONV BRREMatchRef(TBRRERegExp Instance, char* Input, int32_t InputIsUTF8, int32_t InputCodePointLen, int32_t InputCodeUnitLen, void** Captures, int32_t MaxCaptures, int32_t StartCodePoint, int32_t StartCodeUnit, int32_t* LastIndexCodePoint, int32_t* LastIndexCodeUnit, char** Error);
int32_t brreCALLCONV BRRETest(TBRRERegExp Instance, char* Input, int32_t InputIsUTF8, int32_t InputCodePointLen, int32_t InputCodeUnitLen, int32_t StartCodePoint, int32_t StartCodeUnit, int32_t* LastIndexCodePoint, int32_t* LastIndexCodeUnit, char** Error);
int32_t brreCALLCONV BRREFind(TBRRERegExp Instance, char* Input, int32_t InputIsUTF8, int32_t InputCodePointLen, int32_t InputCodeUnitLen, int32_t StartCodePoint, int32_t StartCodeUnit, int32_t* LastIndexCodePoint, int32_t* LastIndexCodeUnit, char** Error);
int32_t brreCALLCONV BRRESplit(TBRRERegExp Instance, char* Input, int32_t InputIsUTF8, int32_t InputCodePointLen, int32_t InputCodeUnitLen, void** Output, int32_t StartCodePoint, int32_t StartCodeUnit, int32_t Limit, int32_t* LastIndexCodePoint, int32_t* LastIndexCodeUnit, char** Error);
int32_t brreCALLCONV BRREReplace(TBRRERegExp Instance, char* Input, char* Replacement, int32_t InputIsUTF8, int32_t InputCodePointLen, int32_t InputCodeUnitLen, int32_t ReplacementIsUTF8, int32_t ReplacementCodePointLen, int32_t ReplacementCodeUnitLen, int32_t StartCodePoint, int32_t StartCodeUnit, int32_t Limit, int32_t* LastIndexCodePoint, int32_t* LastIndexCodeUnit, char** ResultStr, char** Error);
int32_t brreCALLCONV BRREBenchmark(TBRRERegExp Instance, char* Input, int32_t InputIsUTF8, int32_t InputCodePointLen, int32_t InputCodeUnitLen, int32_t StartCodePoint, int32_t StartCodeUnit, int32_t Limit, int32_t* LastIndexCodePoint, int32_t* LastIndexCodeUnit, char** Error);
void brreCALLCONV BRREFree(TBRRERegExp Instance, void* p);
void brreCALLCONV BRREDestroy(TBRRERegExp Instance);

#ifdef __cplusplus
}
#endif

#endif
