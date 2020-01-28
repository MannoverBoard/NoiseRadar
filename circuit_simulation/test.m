filename = 'test.spice';
[text] = SpiceParser.readFile(filename);
[lines,title] = SpiceParser.parseText(text);

[token_groups,errors] = SpiceParser.tokenizeLines(lines);
[declarations,errors,warnings] = SpiceParser.declarationsFromTokens(token_groups);