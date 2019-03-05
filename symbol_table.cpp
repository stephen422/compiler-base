// ref: https://stackoverflow.com/a/12996028
static inline uint64_t hash(const void *p) {
    uint64_t x = (uint64_t)p;
    x = (x ^ (x >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
    x = (x ^ (x >> 27)) * UINT64_C(0x94d049bb133111eb);
    x = x ^ (x >> 31);
    return x;
}

template <typename T>
SymbolTable<T>::SymbolTable() {
    for (int i = 0; i < symbol_table_key_size; i++) {
        keys[i] = nullptr;
    }
}

template <typename T>
SymbolTable<T>::~SymbolTable() {
    for (int i = 0; i < symbol_table_key_size; i++) {
        Symbol<T> *p = keys[i];
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
T *SymbolTable<T>::insert(std::pair<Name *, const T &> pair) {
    // Memory for T is stored inside the symbol
    // FIXME: bad allocator
    Symbol<T> *head = new Symbol<T>(pair.first, pair.second);

    // Insert into the bucket
    int index = hash(pair.first) % symbol_table_key_size;
    Symbol<T> **p = &keys[index];
    head->next = *p;
    *p = head;
    return &head->value;
}

template <typename T>
T *SymbolTable<T>::find(Name *name) const {
    int index = hash(name) % symbol_table_key_size;
    for (Symbol<T> *s = keys[index]; s; s = s->next) {
        if (s->name == name) {
            return &s->value;
        }
    }
    return nullptr;
}

template <typename T>
void SymbolTable<T>::print() const {
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
}

