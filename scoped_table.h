#ifndef SCOPED_TABLE_H
#define SCOPED_TABLE_H

#include <array>

namespace cmp {

struct Name;

// Scoped symbol table.
constexpr int SYMBOL_TABLE_BUCKET_COUNT = 512;
template <typename T> struct ScopedTable {
  struct Symbol {
    Name *name;              // name of this symbol
    T value;                 // semantic value of this symbol
    Symbol *next = nullptr;  // next symbol in the hash table bucket
    Symbol *cross = nullptr; // next symbol in the same scope
    int scope_level = 0;

    Symbol(Name *n, const T &v) : name(n), value(v) {}
  };

  ScopedTable();
  ~ScopedTable();
  T *insert(Name *name, const T &value);

  // find(nullptr) always returns nullptr.
  Symbol *find(const Name *name) const;

  void print() const;

  // Start a new scope.
  void scope_open();
  // Close current cope.
  void scope_close();

  std::array<Symbol *, SYMBOL_TABLE_BUCKET_COUNT> keys;
  std::vector<Symbol *> scope_stack = {};
  int curr_scope_level = 0;
};

// ref: https://stackoverflow.com/a/12996028
static inline uint64_t hash(const void *p) {
  uint64_t x = (uint64_t)p;
  x = (x ^ (x >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
  x = (x ^ (x >> 27)) * UINT64_C(0x94d049bb133111eb);
  x = x ^ (x >> 31);
  return x;
}

template <typename T> ScopedTable<T>::ScopedTable() {
  for (int i = 0; i < SYMBOL_TABLE_BUCKET_COUNT; i++) {
    keys[i] = nullptr;
  }
  scope_stack.push_back(nullptr);
}

template <typename T> ScopedTable<T>::~ScopedTable() {
  for (int i = 0; i < SYMBOL_TABLE_BUCKET_COUNT; i++) {
    Symbol *p = keys[i];
    if (!p) {
      continue;
    }
    while (p) {
      auto *next = p->next;
      delete p;
      p = next;
    }
  }
}

// Insert symbol at the current scope level.
template <typename T> T *ScopedTable<T>::insert(Name *name, const T &value) {
  // memory for T is stored inside the symbol
  // TODO: better allocator
  Symbol *head = new Symbol(name, value);

  // insert into the bucket
  int index = hash(name) % SYMBOL_TABLE_BUCKET_COUNT;
  Symbol **p = &keys[index];
  head->next = *p;
  *p = head;

  // set the scope chain
  head->cross = scope_stack.back();
  head->scope_level = curr_scope_level;
  scope_stack.back() = head;
  return &head->value;
}

template <typename T>
typename ScopedTable<T>::Symbol *ScopedTable<T>::find(const Name *name) const {
  if (!name) return nullptr;

  int index = hash(name) % SYMBOL_TABLE_BUCKET_COUNT;
  for (Symbol *s = keys[index]; s; s = s->next) {
    if (s->name == name) return s;
  }

  return nullptr;
}

template <typename T> void ScopedTable<T>::scope_open() {
  scope_stack.push_back(nullptr);
  curr_scope_level++;
}

template <typename T> void ScopedTable<T>::scope_close() {
  Symbol *p = scope_stack.back();
  while (p) {
    // XXX: does this work with p->name = nullptr?
    int index = hash(p->name) % SYMBOL_TABLE_BUCKET_COUNT;
    keys[index] = p->next;
    auto cross = p->cross;
    delete p;
    p = cross;
  }
  scope_stack.pop_back();
  curr_scope_level--;
}

template <typename T> void ScopedTable<T>::print() const {
  for (int i = 0; i < SYMBOL_TABLE_BUCKET_COUNT; i++) {
    auto *p = keys[i];
    if (!p) continue;

    printf("[%d]", i);
    for (; p; p = p->next) {
      printf("{%s}", p->value.str());
    }
    printf("\n");
  }
  for (size_t i = 0; i < scope_stack.size(); i++) {
    printf("Scope %d:", i);
    for (Symbol *p = scope_stack[i]; p; p = p->cross) {
      printf("{%s}", p->value.str());
    }
    printf("\n");
  }
}

} // namespace cmp

#endif
