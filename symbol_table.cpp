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
        auto *p = keys[i];
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
void SymbolTable<T>::insert(const Symbol<T> sym) {
    int index = hash(sym.name) % symbol_table_key_size;
    auto **p = &keys[index];
    auto *head = new Symbol<T>(sym);
    head->next = *p;
    *p = head;
}

template <typename T>
T *SymbolTable<T>::find(Name *name) const {
    int index = hash(name) % symbol_table_key_size;
    for (auto *s = keys[index]; s; s = s->next) {
        if (s->name == name) {
            return &s->value;
        }
    }
    return nullptr;
}

template <typename T>
void SymbolTable<T>::print() const {
    std::cout << "==== Symbol table ====\n";
    for (int i = 0; i < symbol_table_key_size; i++) {
        auto *p = keys[i];
        if (!p) {
            continue;
        }
        std::cout << "[" << i << "]";
        while (p) {
            std::cout << " {" << p->name->text << "}";
            p = p->next;
        }
        std::cout << std::endl;
    }
}

