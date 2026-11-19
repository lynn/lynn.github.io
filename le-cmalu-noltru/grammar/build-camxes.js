// Builds ../camxes.js from camxes-beta.peg. Based on ilmentufa's build-camxes.js.
//   cd grammar && npm install && npm run build
const fs = require('fs');
const path = require('path');
const pegjs = require('pegjs');
const { convert_file } = require('./pegjs_conv.js');

const pegjsSrc = convert_file(path.join(__dirname, 'camxes-beta.peg')); // writes camxes.pegjs
const parser = pegjs.buildParser(fs.readFileSync(pegjsSrc, 'utf8'), {
  cache: true,
  output: 'source',
  allowedStartRules: ['text'],
});
const banner = `// camxes, built from grammar/camxes-beta.peg by grammar/build-camxes.js. Do not edit.
// Grammar and tools from https://github.com/lojban/ilmentufa (MIT License,
// Copyright (c) 2014 lagleki, ilmen, and other contributors).
`;
fs.writeFileSync(path.join(__dirname, '..', 'camxes.js'), `${banner}var camxes = ${parser};\n`);
fs.unlinkSync(pegjsSrc);
console.log('wrote camxes.js');
