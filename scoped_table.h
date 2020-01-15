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
    for (int i = 0; i < symbol_table_key_size; i++) {
        keys[i] = nullptr;
    }
    scope_open();
}

template <typename T>
ScopedTable<T>::~ScopedTable() {
    for (int i = 0; i < symbol_table_key_size; i++) {
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
T *ScopedTable<T>::insert(std::pair<Name *, const T &> pair) {
    // Memory for T is stored inside the symbol
    // FIXME: bad allocator
    Symbol *head = new Symbol(pair.first, pair.second);

    // Insert into the bucket
    int index = hash(pair.first) % symbol_table_key_size;
    Symbol **p = &keys[index];
    head->next = *p;
    *p = head;

    // Set the scope chain
    head->scope_level = scope_stack.size() - 1;
    head->cross = scope_stack.back();
    scope_stack.back() = head;
    return &head->value;
}

template <typename T>
T *ScopedTable<T>::find(Name *name) const {
    int index = hash(name) % symbol_table_key_size;
    for (Symbol *s = keys[index]; s; s = s->next) {
        if (s->name == name) {
            return &s->value;
        }
    }
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
        int index = hash(p->name) % symbol_table_key_size;
        keys[index] = p->next;
        auto cross = p->cross;
        delete p;
        p = cross;
    }
    scope_stack.pop_back();
    scope_level--;
}

template <typename T>
void ScopedTable<T>::print() const {
    for (int i = 0; i < symbol_table_key_size; i++) {
        auto *p = keys[i];
        if (!p) {
            continue;
        }
        std::cout << "[" << i << "]";
        while (p) {
            std::cout << " {" << p->value.to_string() << "}";
            p = p->next;
        }
        std::cout << std::endl;
    }
    for (size_t i = 0; i < scope_stack.size(); i++) {
        std::cout << "Scope " << i << ":";
        for (Symbol *p = scope_stack[i]; p; p = p->cross) {
            std::cout << " {" << p->value.to_string() << "}";
        }
        std::cout << std::endl;
    }
}

