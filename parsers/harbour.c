/*
*   $Id$
*
*   Copyright (c) 2010, Xavi <jarabal/at/gmail.com>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for the
*   Harbour, Clipper, xBase dialects.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <ctype.h>

#include "entry.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"
#include "routines.h"
#include "x-cpreprocessor.h"

/*
*   DATA DEFINITIONS
*/

#define NEWLINE '\n'
#define BACKSLASH '\\'
#define SPACE ' '

typedef enum {
	K_CLASS, K_DEFINE, K_FUNCTION,
	K_LOCAL, K_MEMBER, K_PROTOTYPE, K_VARIABLE
} hbrKind;

typedef enum eVisibilityHbrType {
	ACCESS_NONE,
	ACCESS_PRIVATE,
	ACCESS_PROTECTED,
	ACCESS_PUBLIC
} accessHbrType;

static kindDefinition HbrKinds [] = {
	{ true,  'c', "class",      "classes"},
	{ true,  'd', "macro",      "macro C definitions"},
	{ true,  'f', "function",   "function, procedure definitions"},
	{ false, 'l', "local",      "local variables (parameters, private, field)"},
	{ true,  'm', "member",     "class members"},
	{ true,  'p', "prototype",  "class function, method, message prototypes"},
	{ true,  'v', "variable",   "variable definitions (public, static)"},
};

static vString *HbrLine;
static bool isDisabledCode;
static bool isDisabledCodeDump;

/*
*   FUNCTION DEFINITIONS
*/

static const unsigned char *skipSpace (const unsigned char *cp)
{
	while (isspace ((int) *cp))
		++cp;

	return cp;
}

static void CheckDisabledCode (void)
{
	const unsigned char *cp = (const unsigned char*) vStringValue (HbrLine);

	if (*cp == '#')
	{
		cp = skipSpace (++cp);
		if (strncasecmp ((const char*) cp, "pragma", 6) == 0 && isspace ((int) cp [6]))
		{
			cp += 7;
			cp = skipSpace (cp);
			if ((strncasecmp ((const char*) cp, "begindump", 9) == 0 && isspace ((int) cp [9])) ||
				(strncasecmp ((const char*) cp, "__cstream", 9) == 0 && isspace ((int) cp [9])))
			{
				isDisabledCode = true;
				isDisabledCodeDump = (*cp != '_');
			}
			else if ((strncasecmp ((const char*) cp, "enddump", 7) == 0 && isspace ((int) cp [7])) ||
					(strncasecmp ((const char*) cp, "__endtext", 9) == 0 && isspace ((int) cp [9])))
				isDisabledCode = isDisabledCodeDump = false;
		}
	}
}

static const unsigned char *fileReadHbrLine (void)
{
	int c;
	int i;
	bool isComment;
	bool isNextLine = false;
	const unsigned char *line = NULL;

	do
	{
		int k = 0;
		bool isCommentLine = false;

		i = 0;
		isComment = false;
		vStringClear (HbrLine);
		do
		{
			c = cppGetc ();
		if (c == EOF)
			break;
		else if (c == NEWLINE)
		{
			vStringStripTrailing (HbrLine);
			i = vStringLength (HbrLine);
			CheckDisabledCode ();
			if (i && ! isDisabledCode)
			{
				k = vStringChar (HbrLine, i - 1);
				if (k == ';' || k == BACKSLASH)
				{
					k = 0;
					isNextLine = true;
					isCommentLine = false;
					vStringChar (HbrLine, i - 1) = BACKSLASH;
					continue;
				}
			}
				break;
			}
			else if (c == '*' && ! isNextLine && k == 0)
				isComment = true;
			else if (c == '&')
			{
			int u = cppGetc ();

			if (u == '&')
				isCommentLine = true;
			else
				cppUngetc (u);
			}

			if (! isspace (c))
				++k;

		if (k && ! isComment && ! isCommentLine)
		{
			vStringPut (HbrLine, c);
			++i;
		}
	} while (true);
} while ((isComment || i == 0) && c != EOF);

	if (i || c == NEWLINE)
	{
		if (isNextLine)
		{
			do
				if (vStringChar (HbrLine, --i) == BACKSLASH)
					vStringChar (HbrLine, i) = SPACE;
			while (i);
		}
		line = (const unsigned char*) vStringValue (HbrLine);
	}

	return line;
}

static const unsigned char *skipToMatch (const unsigned char *cp, const char *const pair)
{
	int i = 1;
	const unsigned char begin = pair [0], end = pair [1];

	while (i && *cp)
	{
		++cp;
		if (*cp == begin)
			++i;
		else if (*cp == end)
			--i;
	}

	return cp;
}

static bool isIWordChar (int ch)
{
	return (bool) (isalnum(ch) || ch == '_');
}

static const unsigned char *parseIdentifier (const unsigned char *cp, vString *const identifier)
{
	vStringClear (identifier);
	while (isIWordChar ((int) *cp))
	{
		vStringPut (identifier, (int) *cp);
		++cp;
	}

	return cp;
}

static const unsigned char *parseCommaIdentifier (const unsigned char *cp, vString *const identifier)
{
	cp = skipSpace (cp);
	cp = parseIdentifier (cp, identifier);
	for (; *cp && *cp != ','; ++cp)
	{
		switch (*cp)
		{
			case '(': cp = skipToMatch (cp, "()"); break;
			case '{': cp = skipToMatch (cp, "{}"); break;
			case '[': cp = skipToMatch (cp, "[]");
		}
	}
	if (*cp == ',')
		++cp;

	return cp;
}

static const char *accessHbrString (const accessHbrType access)
{
	static const char *const names [] = {
		"?", "private", "protected", "public"
	};

	return names [(int) access];
}

static void makeTag (vString *const name,
				vString *const extension,
				vString *const ClassName,
				const accessHbrType access,
				bool isFileScope, const hbrKind type)
{
	if (vStringLength (name) > 0)
	{
	    tagEntryInfo e;
	    initTagEntry (&e, vStringValue (name), type);

		e.isFileScope  = isFileScope;

		if (extension != NULL && vStringLength (extension) > 0)
		{
			if (type == K_FUNCTION || type == K_PROTOTYPE)
				e.extensionFields.signature = vStringValue (extension);
			else if (type == K_CLASS)
				e.extensionFields.inheritance = vStringValue (extension);
		}

		if (ClassName != NULL && vStringLength (ClassName) > 0)
		{
			e.extensionFields.scopeKindIndex = K_CLASS;
			e.extensionFields.scopeName = vStringValue (ClassName);
		}

		if (access != ACCESS_NONE)
			e.extensionFields.access = accessHbrString (access);

	    makeTagEntry (&e);
	}
}

static accessHbrType accessHbrField (const unsigned char *cp, const accessHbrType accessScope)
{
	accessHbrType access = accessScope;

	if (strncasecmp ((const char*) cp, "hidden", 6) == 0)
		access = ACCESS_PRIVATE;
	else if (strncasecmp ((const char*) cp, "protected", 9) == 0)
		access = ACCESS_PROTECTED;
	else if (strncasecmp ((const char*) cp, "exported", 8) == 0)
		access = ACCESS_PUBLIC;
	else if (strncasecmp ((const char*) cp, "export", 6) == 0)
		access = ACCESS_PUBLIC;
	else if (strncasecmp ((const char*) cp, "visible", 7) == 0)
		access = ACCESS_PUBLIC;
	else if (strncasecmp ((const char*) cp, "public", 6) == 0)
		access = ACCESS_PUBLIC;
	else if (strncasecmp ((const char*) cp, "private", 7) == 0)
		access = ACCESS_PRIVATE;
	else if (strncasecmp ((const char*) cp, "reserved", 8) == 0)
		access = ACCESS_PUBLIC;
	else if (strncasecmp ((const char*) cp, "published", 9) == 0)
		access = ACCESS_PUBLIC;

	return access;
}

static accessHbrType accessLastWord (const accessHbrType accessScope)
{
	int i = vStringLength (HbrLine);
	const unsigned char *cp = (const unsigned char*) vStringValue (HbrLine);

	while (i && vStringChar (HbrLine, --i) != SPACE) {};
	cp += i + 1;

	return accessHbrField (cp, accessScope);
}

static void findHbrTags (void)
{
	const unsigned char *cp;
	vString *name = vStringNew();
	vString *ClassName = vStringNew();
	bool isClassImpl = true;
	bool isFileScope = false;
	bool isStaticLocal = false;
	accessHbrType accessScope = ACCESS_NONE;

	cppInit (false, false, false, false,
			 KIND_GHOST_INDEX, 0, 0,
			 KIND_GHOST_INDEX, 0, 0,
			 KIND_GHOST_INDEX,
			 FIELD_UNKNOWN);

	HbrLine = vStringNew ();
	isDisabledCode = false;
	isDisabledCodeDump = false;
	while ((cp = fileReadHbrLine()) != NULL)
	{
		int i;
		bool isStatic = false;

		cp = skipSpace (cp);
		if (isDisabledCode)
		{
			if (isDisabledCodeDump && (strncmp ((const char*) cp, "HB_FUNC_STATIC", (size_t) (i = 14)) == 0 ||
				strncmp ((const char*) cp, "HB_FUNC_INIT", (size_t) (i = 12)) == 0 ||
				strncmp ((const char*) cp, "HB_FUNC_EXIT", (size_t) i) == 0 ||
				strncmp ((const char*) cp, "HB_FUNC", (size_t) (i = 7)) == 0))
			{
				cp += i;
				cp = skipSpace (cp);
				if (*cp == '(')
				{
					cp = skipSpace (++cp);
					cp = parseIdentifier (cp, name);
					makeTag (name, NULL, NULL,
								(i == 7 ? ACCESS_NONE : (i == 14 ? ACCESS_PRIVATE : ACCESS_PROTECTED)),
								(i != 7), K_FUNCTION);
					vStringClear (name);
				}
			}
			continue;
		}
		else if ((strncasecmp ((const char*) cp, "class", (size_t) (i = 5)) == 0 && isspace ((int) cp [i])) ||
				(strncasecmp ((const char*) cp, "create class", (size_t) (i = 12)) == 0 && isspace ((int) cp [i])))
		{
			vString *Inheritance = NULL;

			isClassImpl = false;
			isFileScope = false;
			cp += i + 1;
			cp = skipSpace (cp);
			cp = parseIdentifier (cp, ClassName);
			cp = skipSpace (cp);
			if ((strncasecmp ((const char*) cp, "from", (size_t) (i = 4)) == 0 && isspace ((int) cp [i])) ||
				(strncasecmp ((const char*) cp, "inherit", (size_t) (i = 7)) == 0 && isspace ((int) cp [i])))
			{
				cp += i + 1;
				Inheritance = vStringNew();
				do
				{
					cp = skipSpace (cp);
					while (isIWordChar ((int) *cp) || *cp == ',')
					{
						i = (int) *cp;
						vStringPut (Inheritance, i);
						++cp;
					}
				cp = skipSpace (cp);
			} while (*cp == ',' || i == ',');
			}
			makeTag (ClassName, Inheritance, NULL, ACCESS_NONE, false, K_CLASS);
			vStringDelete (Inheritance);
			accessScope = ACCESS_PUBLIC;
			continue;
		}
		else if ((strncasecmp ((const char*) cp, "hidden:", 7) == 0) ||
				(strncasecmp ((const char*) cp, "public:", 7) == 0) ||
				(strncasecmp ((const char*) cp, "export:", 7) == 0) ||
				(strncasecmp ((const char*) cp, "exported:", 9) == 0) ||
				(strncasecmp ((const char*) cp, "reserved:", 9) == 0) ||
				(strncasecmp ((const char*) cp, "protected:", 10) == 0) ||
				(strncasecmp ((const char*) cp, "published:", 10) == 0) ||
				(strncasecmp ((const char*) cp, "visible:", 8) == 0) ||
				(strncasecmp ((const char*) cp, "private:", 8) == 0))
		{
			accessScope = accessHbrField (cp, accessScope);
			continue;
		}
		else if ((strncasecmp ((const char*) cp, "var", (size_t) (i = 3)) == 0 && isspace ((int) cp [i])) ||
				(strncasecmp ((const char*) cp, "data", (size_t) (i = 4)) == 0 && isspace ((int) cp [i])) ||
				(strncasecmp ((const char*) cp, "local", (size_t) (i = 5)) == 0 && isspace ((int) cp [i])) ||
				(strncasecmp ((const char*) cp, "field", (size_t) i) == 0 && isspace ((int) cp [i])) ||
				(strncasecmp ((const char*) cp, "public", (size_t) (i = 6)) == 0 && isspace ((int) cp [i])) ||
				(strncasecmp ((const char*) cp, "private", (size_t) (i = 7)) == 0 && isspace ((int) cp [i])) ||
				(strncasecmp ((const char*) cp, "property", (size_t) (i = 8)) == 0 && isspace ((int) cp [i])) ||
				(strncasecmp ((const char*) cp, "classvar", (size_t) i) == 0 && isspace ((int) cp [i])) ||
				(strncasecmp ((const char*) cp, "classdata", (size_t) (i = 9)) == 0 && isspace ((int) cp [i])) ||
				(strncasecmp ((const char*) cp, "component", (size_t) i) == 0 && isspace ((int) cp [i])) ||
				(strncasecmp ((const char*) cp, "parameters", (size_t) (i = 10)) == 0 && isspace ((int) cp [i])))
		{
			/* Local: local=5, field=5, private=7, parameters=10. Variables: public=6. Members: the rest. */
			const hbrKind type = (i == 5 || i == 7 || i == 10 ? K_LOCAL : (i == 6 ? K_VARIABLE : K_MEMBER));

			cp += i + 1;
			do
			{
				cp = parseCommaIdentifier (cp, name);
				makeTag (name, NULL, (type == K_MEMBER ? ClassName : NULL),
							(type == K_MEMBER ? accessLastWord (accessScope) : ACCESS_NONE),
							(i == 6 ? false : isFileScope), type);
				vStringClear (name);
			} while (*cp);
			continue;
		}
		else if (strncasecmp ((const char*) cp, "endclass", 8) == 0)
		{
			isClassImpl = true;
			accessScope = ACCESS_NONE;
			vStringClear (ClassName);
			continue;
		}
		else if ((strncasecmp ((const char*) cp, "static", (size_t) (i = 6)) == 0 && isspace ((int) cp [i])) ||
			(strncasecmp ((const char*) cp, "init", (size_t) (i = 4)) == 0 && isspace ((int) cp [i])) ||
			(strncasecmp ((const char*) cp, "exit", (size_t) i) == 0 && isspace ((int) cp [i])) ||
			(strncasecmp ((const char*) cp, "thread static", (size_t) (i = 13)) == 0 && isspace ((int) cp [i])))
		{
			cp += i + 1;
			isStatic = true;
			accessScope = (i == 4 ? ACCESS_PROTECTED : ACCESS_PRIVATE);
		}

		cp = skipSpace (cp);
		if ((strncasecmp ((const char*) cp, "func", (size_t) (i = 4)) == 0 && isspace ((int) cp [i])) ||
			(strncasecmp ((const char*) cp, "proc", (size_t) i) == 0 && isspace ((int) cp [i])) ||
			(strncasecmp ((const char*) cp, "event", (size_t) (i = 5)) == 0 && isspace ((int) cp [i])) ||
			(strncasecmp ((const char*) cp, "function", (size_t) (i = 8)) == 0 && isspace ((int) cp [i])) ||
			(strncasecmp ((const char*) cp, "on error", (size_t) i) == 0 && isspace ((int) cp [i])) ||
			(strncasecmp ((const char*) cp, "procedure", (size_t) (i = 9)) == 0 && isspace ((int) cp [i])) ||
			(strncasecmp ((const char*) cp, "method procedure", (size_t) (i = 16)) == 0 && isspace ((int) cp [i])) ||
			(strncasecmp ((const char*) cp, "method", (size_t) (i = 6)) == 0 && isspace ((int) cp [i])) ||
			(strncasecmp ((const char*) cp, "access", (size_t) i) == 0 && isspace ((int) cp [i])) ||
			(strncasecmp ((const char*) cp, "assign", (size_t) i) == 0 && isspace ((int) cp [i])) ||
			(strncasecmp ((const char*) cp, "message", (size_t) (i = 7)) == 0 && isspace ((int) cp [i])) ||
			(strncasecmp ((const char*) cp, "destructor", (size_t) (i = 10)) == 0 && isspace ((int) cp [i])) ||
			(strncasecmp ((const char*) cp, "constructor", (size_t) (i = 11)) == 0 && isspace ((int) cp [i])) ||
			(strncasecmp ((const char*) cp, "error handler", (size_t) (i = 13)) == 0 && isspace ((int) cp [i])))
		{
			vString *Signature = NULL;

			isFileScope = isStatic;
			isStaticLocal = true;
			cp += i + 1;
			cp = skipSpace (cp);
			cp = parseIdentifier (cp, name);
			if (*cp == ':' && isClassImpl)
			{
				vStringCopy (ClassName, name);
				cp = parseIdentifier (++cp, name);
			}

			cp = skipSpace (cp);
			if (*cp == '(')
			{
			Signature = vStringNew();
			do
				vStringPut (Signature, (int) *cp);
			while (*cp++ != ')' && *cp);
				if (isClassImpl)
				{
					vString *Parameter = vStringNew();
					const unsigned char *par = (const unsigned char*) vStringValue (Signature);

					++par;
					do
					{
						par = parseCommaIdentifier (par, Parameter);
						makeTag (Parameter, NULL, NULL, ACCESS_NONE, isFileScope, K_LOCAL);
						vStringClear (Parameter);
					} while (*par);
					vStringDelete (Parameter);

					cp = skipSpace (cp);
					if (strncasecmp ((const char*) cp, "class", 5) == 0 && isspace ((int) cp [5]))
						cp = parseIdentifier (cp + 6, ClassName);
				}
			}
			makeTag (name, Signature, ClassName, (isClassImpl ? accessScope : accessLastWord (accessScope)),
						isFileScope, (isClassImpl ? K_FUNCTION : K_PROTOTYPE));
			vStringClear (name);
			vStringDelete (Signature);
			if (isClassImpl)
				vStringClear (ClassName);
			if (isStatic)
				accessScope = ACCESS_NONE;
		}
		else if (isStatic)
		{
			do
			{
				cp = parseCommaIdentifier (cp, name);
				makeTag (name, NULL, NULL, (isStaticLocal ? ACCESS_NONE : accessScope),
							(isStaticLocal ? isFileScope : true),
							(isStaticLocal ? K_LOCAL : K_VARIABLE));
				vStringClear (name);
			} while (*cp);
			accessScope = ACCESS_NONE;
		}
	}
	vStringDelete (HbrLine);

	cppTerminate ();

	vStringDelete (ClassName);
	vStringDelete (name);
}

extern parserDefinition* HbrParser (void)
{
	static const char *const extensions [] = { "prg", "hrb", "ch", NULL };
	parserDefinition* def = parserNew ("Harbour");
	def->kindTable  = HbrKinds;
	def->kindCount  = ARRAY_SIZE (HbrKinds);
	def->extensions = extensions;
	def->parser     = findHbrTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
