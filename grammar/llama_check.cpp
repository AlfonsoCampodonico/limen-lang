// Check Tessera programs against grammar/tessera.gbnf with llama.cpp's own GBNF
// parser and matcher, the code that constrains decoding. Built and run by
// grammar/llama-check.sh, against a pinned llama.cpp release.
//
// usage: llama_check GRAMMAR FILE... [--reject FILE...]
// Files before --reject must be accepted, files after it rejected. A file is
// accepted when the whole text matches the root rule, the same test as
// llama.cpp's tests/test-gbnf-validator.cpp.

#include "../src/llama-grammar.h"
#include "../src/unicode.h"

#include <cstdio>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

static std::string read_file(const char * path) {
    std::ifstream f(path, std::ios::binary);
    std::stringstream b;
    b << f.rdbuf();
    return b.str();
}

// Whether [text] matches; on failure, the code point where matching stopped.
static bool matches(const std::string & grammar_text, const std::string & text, size_t & where) {
    llama_grammar * g = llama_grammar_init_impl(nullptr, grammar_text.c_str(), "root", false, nullptr, 0, nullptr, 0);
    if (g == nullptr) {
        fprintf(stderr, "llama.cpp could not parse the grammar\n");
        exit(2);
    }
    const auto & stacks = llama_grammar_get_stacks(g);
    bool ok = true;
    where = 0;
    for (const auto cpt : unicode_cpts_from_utf8(text)) {
        llama_grammar_accept(g, cpt);
        if (stacks.empty()) {
            ok = false;
            break;
        }
        ++where;
    }
    if (ok) {
        ok = false;
        for (const auto & s : stacks) {
            if (s.empty()) {
                ok = true;
                break;
            }
        }
    }
    llama_grammar_free_impl(g);
    return ok;
}

// line:column of code point [cp] in [text]
static std::string position(const std::string & text, size_t cp) {
    const auto cpts = unicode_cpts_from_utf8(text);
    size_t line = 1, col = 1;
    for (size_t i = 0; i < cp && i < cpts.size(); i++) {
        if (cpts[i] == '\n') {
            line++;
            col = 1;
        } else {
            col++;
        }
    }
    return std::to_string(line) + ":" + std::to_string(col);
}

int main(int argc, char ** argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: %s GRAMMAR FILE... [--reject FILE...]\n", argv[0]);
        return 2;
    }
    const std::string grammar_text = read_file(argv[1]);
    bool reject = false;
    int total = 0, good = 0;
    for (int i = 2; i < argc; i++) {
        if (std::string(argv[i]) == "--reject") {
            reject = true;
            continue;
        }
        const std::string text = read_file(argv[i]);
        size_t where;
        const bool ok = matches(grammar_text, text, where);
        total++;
        if (ok != reject) {
            good++;
        } else if (reject) {
            printf("WRONGLY ACCEPTED %s\n", argv[i]);
        } else {
            printf("REJECTED %s at %s\n", argv[i], position(text, where).c_str());
        }
    }
    printf("llama.cpp: %d/%d as expected\n", good, total);
    return good == total ? 0 : 1;
}
