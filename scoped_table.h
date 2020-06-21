#ifndef SCOPED_TABLE_H
#define SCOPED_TABLE_H

#include <array>

namespace cmp {

// Scoped symbol table.
constexpr int SYMBOL_TABLE_BUCKET_COUNT = 512;
template <typename Key, typename T> struct ScopedTable {
  struct Symbol {
    const Key key;           // name of this symbol
    T value;                 // semantic value of this symbol
    Symbol *next = nullptr;  // next symbol in the hash table bucket
    Symbol *cross = nullptr; // next symbol in the same scope
    int scope_level = 0;

    Symbol(Key k, const T &v) : key(k), value(v) {}
  };

  ScopedTable();
  ~ScopedTable();
  T *insert(const Key key, const T &value);

  // find(nullptr) always returns nullptr.
  Symbol *find(const Key key) const;

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
  static_assert(sizeof(p) == sizeof(uint64_t));
  uint64_t x = (uint64_t)p;
  x = (x ^ (x >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
  x = (x ^ (x >> 27)) * UINT64_C(0x94d049bb133111eb);
  x = x ^ (x >> 31);
  return x;
}

template <typename Key, typename T> ScopedTable<Key, T>::ScopedTable() {
  for (int i = 0; i < SYMBOL_TABLE_BUCKET_COUNT; i++) {
    keys[i] = nullptr;
  }
  scope_stack.push_back(nullptr);
}

template <typename Key, typename T> ScopedTable<Key, T>::~ScopedTable() {
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
template <typename Key, typename T>
T *ScopedTable<Key, T>::insert(const Key key, const T &value) {
  // memory for T is stored inside the symbol
  // TODO: better allocator
  Symbol *head = new Symbol(key, value);

  // insert into the bucket
  int index = hash(key) % SYMBOL_TABLE_BUCKET_COUNT;
  Symbol **p = &keys[index];
  head->next = *p;
  *p = head;

  // set the scope chain
  head->cross = scope_stack.back();
  head->scope_level = curr_scope_level;
  scope_stack.back() = head;
  return &head->value;
}

template <typename Key, typename T>
typename ScopedTable<Key, T>::Symbol *
ScopedTable<Key, T>::find(const Key key) const {
  if (!key) return nullptr;

  int index = hash(key) % SYMBOL_TABLE_BUCKET_COUNT;
  for (Symbol *s = keys[index]; s; s = s->next) {
    if (s->key == key) return s;
  }

  return nullptr;
}

template <typename Key, typename T> void ScopedTable<Key, T>::scope_open() {
  scope_stack.push_back(nullptr);
  curr_scope_level++;
}

template <typename Key, typename T> void ScopedTable<Key, T>::scope_close() {
  Symbol *p = scope_stack.back();
  while (p) {
    // XXX: does this work with p->key = nullptr?
    int index = hash(p->key) % SYMBOL_TABLE_BUCKET_COUNT;
    keys[index] = p->next;
    auto cross = p->cross;
    delete p;
    p = cross;
  }
  scope_stack.pop_back();
  curr_scope_level--;
}

template <typename Key, typename T> void ScopedTable<Key, T>::print() const {
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
