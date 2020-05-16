// ref: https://stackoverflow.com/a/12996028
static inline uint64_t hash(const void *p) {
    uint64_t x = (uint64_t)p;
    x = (x ^ (x >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
    x = (x ^ (x >> 27)) * UINT64_C(0x94d049bb133111eb);
    x = x ^ (x >> 31);
    return x;
}

template <typename T>
ScopedTable<T>::ScopedTable() {
    for (int i = 0; i < SYMBOL_TABLE_BUCKET_COUNT; i++) {
        keys[i] = nullptr;
    }
    scope_stack.push_back(nullptr);
}

template <typename T>
ScopedTable<T>::~ScopedTable() {
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

template <typename T>
T *ScopedTable<T>::insert(Name *name, const T &value) {
    // memory for T is stored inside the symbol
    // FIXME: bad allocator
    Symbol *head = new Symbol(name, value);

    // insert into the bucket
    int index = hash(name) % SYMBOL_TABLE_BUCKET_COUNT;
    Symbol **p = &keys[index];
    head->next = *p;
    *p = head;

    // set the scope chain
    head->cross = scope_stack.back();
    head->scope_level = scope_level;
    scope_stack.back() = head;
    return &head->value;
}

template <typename T>
typename ScopedTable<T>::Symbol *ScopedTable<T>::find(Name *name) const {
    int index = hash(name) % SYMBOL_TABLE_BUCKET_COUNT;
    for (Symbol *s = keys[index]; s; s = s->next)
        if (s->name == name)
            return s;
    return nullptr;
}

template <typename T>
void ScopedTable<T>::scope_open() {
    scope_stack.push_back(nullptr);
    scope_level++;
}

template <typename T>
void ScopedTable<T>::scope_close() {
    Symbol *p = scope_stack.back();
    while (p) {
        int index = hash(p->name) % SYMBOL_TABLE_BUCKET_COUNT;
        keys[index] = p->next;
        auto cross = p->cross;
        delete p;
        p = cross;
    }
    scope_stack.pop_back();
    scope_level--;
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
