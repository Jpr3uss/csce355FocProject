#!/bin/sh

echo "no-op"
(in2post|regex --no-op|pre2in) < input.txt > no-op.txt
echo "simplify"
(in2post|regex --simplify|pre2in) < input.txt > simplify.txt
echo "empty"
(in2post|regex --empty) < input.txt > empty.txt
echo "has-epsilon"
(in2post|regex --has-epsilon) < input.txt > has-epsilon.txt
echo "has-nonepsilon"
(in2post|regex --has-nonepsilon) < input.txt > has-nonepsilon.txt
echo "uses a"
(in2post|regex --uses a) < input.txt > uses-a.txt
echo "uses b"
(in2post|regex --uses b) < input.txt > uses-b.txt
echo "uses c"
(in2post|regex --uses c) < input.txt > uses-c.txt
echo "uses d"
(in2post|regex --uses d) < input.txt > uses-d.txt
echo "uses e"
(in2post|regex --uses e) < input.txt > uses-e.txt
echo "uses f"
(in2post|regex --uses f) < input.txt > uses-f.txt
echo "not-using a"
(in2post|regex --not-using a|pre2in) < input.txt > not-using-a.txt
echo "not-using b"
(in2post|regex --not-using b|pre2in) < input.txt > not-using-b.txt
echo "not-using c"
(in2post|regex --not-using c|pre2in) < input.txt > not-using-c.txt
echo "not-using d"
(in2post|regex --not-using d|pre2in) < input.txt > not-using-d.txt
echo "not-using e"
(in2post|regex --not-using e|pre2in) < input.txt > not-using-e.txt
echo "not-using f"
(in2post|regex --not-using f|pre2in) < input.txt > not-using-f.txt
echo "infinite"
(in2post|regex --infinite) < input.txt > infinite.txt
echo "starts-with a"
(in2post|regex --starts-with a) < input.txt > starts-with-a.txt
echo "starts-with b"
(in2post|regex --starts-with b) < input.txt > starts-with-b.txt
echo "starts-with c"
(in2post|regex --starts-with c) < input.txt > starts-with-c.txt
echo "starts-with d"
(in2post|regex --starts-with d) < input.txt > starts-with-d.txt
echo "starts-with e"
(in2post|regex --starts-with e) < input.txt > starts-with-e.txt
echo "starts-with f"
(in2post|regex --starts-with f) < input.txt > starts-with-f.txt
echo "reverse"
(in2post|regex --reverse|pre2in) < input.txt > reverse.txt
echo "ends-with a"
(in2post|regex --ends-with a) < input.txt > ends-with-a.txt
echo "ends-with b"
(in2post|regex --ends-with b) < input.txt > ends-with-b.txt
echo "ends-with c"
(in2post|regex --ends-with c) < input.txt > ends-with-c.txt
echo "ends-with d"
(in2post|regex --ends-with d) < input.txt > ends-with-d.txt
echo "ends-with e"
(in2post|regex --ends-with e) < input.txt > ends-with-e.txt
echo "ends-with f"
(in2post|regex --ends-with f) < input.txt > ends-with-f.txt
echo "prefixes"
(in2post|regex --prefixes|pre2in) < input.txt > prefixes.txt
echo "bs-for-a"
(in2post|regex --bs-for-a|pre2in) < input.txt > bs-for-a.txt
echo "insert a"
(in2post|regex --insert a|pre2in) < input.txt > insert-a.txt
echo "insert b"
(in2post|regex --insert b|pre2in) < input.txt > insert-b.txt
echo "insert c"
(in2post|regex --insert c|pre2in) < input.txt > insert-c.txt
echo "insert d"
(in2post|regex --insert d|pre2in) < input.txt > insert-d.txt
echo "insert e"
(in2post|regex --insert e|pre2in) < input.txt > insert-e.txt
echo "insert f"
(in2post|regex --insert f|pre2in) < input.txt > insert-f.txt
echo "strip a"
(in2post|regex --strip a|pre2in) < input.txt > strip-a.txt
echo "strip b"
(in2post|regex --strip b|pre2in) < input.txt > strip-b.txt
echo "strip c"
(in2post|regex --strip c|pre2in) < input.txt > strip-c.txt
echo "strip d"
(in2post|regex --strip d|pre2in) < input.txt > strip-d.txt
echo "strip e"
(in2post|regex --strip e|pre2in) < input.txt > strip-e.txt
echo "strip f"
(in2post|regex --strip f|pre2in) < input.txt > strip-f.txt

# yes iff the regex matches "abac"
echo "matches-abac"
(in2post|regex --strip a|pre2in|in2post|regex --strip b|pre2in|in2post|regex --strip a|pre2in|in2post|regex --strip c|pre2in|in2post|regex --has-epsilon) < input.txt > matches-abac.txt
