// Copyright (c) 2018 Tyler Reisinger
// Copyright (c) 2025 Michael Cummings
// SPDX-License-Identifier: MIT

#ifndef CPP_RESULT_HPP
#define CPP_RESULT_HPP

#include <cstdint>
#include <functional>
#include <iostream>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>

namespace result {
template <typename T>
using optional = std::optional<T>;
using nullopt_t = std::nullopt_t;
inline constexpr nullopt_t nullopt = std::nullopt;

template <typename T>
std::ostream& operator<<(
        std::ostream& stream, const std::reference_wrapper<T>& obj);

enum class ResultKind : uint8_t {
    Ok = 0,
    Err = 1,
};

template <typename T, typename E>
class [[nodiscard]] Result;

struct ok_tag_t {};

struct err_tag_t {};

struct unit_t {};

inline constexpr auto ok_tag = ok_tag_t{};
inline constexpr auto err_tag = err_tag_t{};
inline constexpr auto unit = unit_t{};

template <typename T>
struct is_result : std::false_type {};

template <typename T, typename E>
struct is_result<Result<T, E>> : std::true_type {};

constexpr bool operator==(unit_t, unit_t) { return true; }

constexpr bool operator!=(unit_t, unit_t) { return false; }

template <typename T>
class Err {
public:
    using value_type = T;

    explicit constexpr Err(const T& val) : m_value(val) {}

    explicit constexpr Err(T&& val) : m_value(std::move(val)) {}

    constexpr const T& value() const& { return m_value; }

    constexpr T&& value() && { return std::move(m_value); }

private:
    T m_value;
};

template <typename T>
class Ok {
public:
    using value_type = T;

    explicit constexpr Ok(const T& val) : m_value(val) {}

    explicit constexpr Ok(T&& val) : m_value(std::move(val)) {}

    constexpr const T& value() const& { return m_value; }

    constexpr T&& value() && { return std::move(m_value); }

    template <typename E>
    explicit constexpr operator Result<T, E>() const& {
        return Result<T, E>(Ok(m_value));
    }

    template <typename E>
    explicit constexpr operator Result<T, E>() && {
        return Result<T, E>(Ok(std::move(m_value)));
    }

private:
    T m_value;
};

template <>
class Ok<unit_t> {
public:
    using value_type = unit_t;

    constexpr Ok() = default;

    explicit constexpr Ok(unit_t) {}

    [[nodiscard]] constexpr unit_t value() const { return {}; }
};

Ok() -> Ok<unit_t>;

namespace details {
inline void terminate(const std::string_view& msg) {
    std::cerr << msg << std::endl;
    std::terminate();
}

template <typename T, typename E>
class ResultStorage {
    using DecayT = std::decay_t<T>;
    using DecayE = std::decay_t<E>;

public:
    using value_type = T;
    using error_type = E;
    using data_type = std::aligned_union_t<1, T, E>;

    ResultStorage() = delete;

    template <typename... Args>
    explicit constexpr ResultStorage(ok_tag_t, Args&&... args) {
        if constexpr(!std::is_same_v<T, unit_t>) {
            new(&m_data) DecayT(std::forward<Args>(args)...);
        }
        m_tag = ResultKind::Ok;
    }

    template <typename... Args>
    explicit constexpr ResultStorage(err_tag_t, Args&&... args) {
        new(&m_data) DecayE(std::forward<Args>(args)...);
        m_tag = ResultKind::Err;
    }

    constexpr explicit ResultStorage(Ok<T> val) {
        if constexpr(!std::is_same_v<T, unit_t>) {
            new(&m_data) DecayT(std::move(val).value());
        }
        m_tag = ResultKind::Ok;
    }

    explicit constexpr ResultStorage(Err<E> val) {
        new(&m_data) DecayE(std::move(val).value());
        m_tag = ResultKind::Err;
    }

    constexpr ResultStorage(const ResultStorage& rhs) noexcept(
            std::is_nothrow_copy_constructible_v<T> &&
            std::is_nothrow_copy_constructible_v<E>)
        : m_tag(rhs.m_tag) {
        if(kind() == ResultKind::Ok) {
            if constexpr(!std::is_same_v<T, unit_t>) {
                new(&m_data) DecayT(rhs.get<T>());
            }
        } else {
            new(&m_data) DecayE(rhs.get<E>());
        }
    }

    constexpr ResultStorage(ResultStorage&& rhs) noexcept(
            std::is_nothrow_move_constructible_v<T> &&
            std::is_nothrow_move_constructible_v<E>)
        : m_tag(rhs.m_tag) {
        if(kind() == ResultKind::Ok) {
            if constexpr(!std::is_same_v<T, unit_t>) {
                new(&m_data) DecayT(std::move(rhs).template get<T>());
            }
        } else {
            new(&m_data) DecayE(std::move(rhs).template get<E>());
        }
    }

    constexpr ResultStorage& operator=(const ResultStorage& rhs) noexcept(
            std::is_nothrow_copy_assignable_v<T> &&
            std::is_nothrow_copy_assignable_v<E>) {
        destroy();
        m_tag = rhs.m_tag;

        if(kind() == ResultKind::Ok) {
            T& val = get<T>();
            val = rhs.get<T>();
        } else {
            E& val = get<E>();
            val = rhs.get<E>();
        }
        return *this;
    }

    constexpr ResultStorage& operator=(ResultStorage&& rhs) noexcept(
            std::is_nothrow_move_assignable_v<T> &&
            std::is_nothrow_move_assignable_v<E>) {
        destroy();
        m_tag = rhs.m_tag;

        if(kind() == ResultKind::Ok) {
            T& val = get<T>();
            val = std::move(rhs).template get<T>();
        } else {
            E& val = get<E>();
            val = std::move(rhs).template get<E>();
        }
        return *this;
    }

    template <typename U>
    constexpr const U& get() const& noexcept {
        static_assert(std::is_same_v<T, U> || std::is_same_v<E, U>);
        return *reinterpret_cast<const U*>(&m_data);
    }

    template <typename U>
    constexpr U& get() & noexcept {
        static_assert(std::is_same_v<T, U> || std::is_same_v<E, U>);
        return *reinterpret_cast<U*>(&m_data);
    }

    template <typename U>
    constexpr U&& get() && noexcept {
        static_assert(std::is_same_v<T, U> || std::is_same_v<E, U>);
        return std::move(*reinterpret_cast<U*>(&m_data));
    }

    [[nodiscard]] constexpr ResultKind kind() const noexcept { return m_tag; }

    ~ResultStorage() { destroy(); }

private:
    void destroy() {
        switch(m_tag) {
        case ResultKind::Ok:
            get<T>().~T();
            break;
        case ResultKind::Err:
            get<E>().~E();
            break;
        }
    }

    data_type m_data;
    ResultKind m_tag;
};
} // namespace details

template <typename T, typename E>
class [[nodiscard]] Result {
public:
    using value_type = T;
    using error_type = E;

    static_assert(std::is_same_v<std::remove_reference_t<T>, T>,
            "Result<T, E> cannot store reference types."
            "Try using `std::reference_wrapper`");
    static_assert(std::is_same_v<std::remove_reference_t<E>, E>,
            "Result<T, E> cannot store reference types."
            "Try using `std::reference_wrapper`");

    static_assert(!std::is_same_v<T, void>,
            "Cannot create a Result<T, E> object with T=void. "
            "Introducing `void` to the type causes a lot of problems, "
            "use the type `unit_t` instead");
    static_assert(!std::is_same_v<E, void>,
            "Cannot create a Result<T, E> object with E=void. You want an "
            "optional<T>.");

    constexpr Result() {
        static_assert(std::is_default_constructible_v<T>,
                "Result<T, E> may only be default constructed if T is default "
                "constructible.");
        m_storage = Ok(T());
    }

    explicit constexpr Result(Ok<T> value) : m_storage(std::move(value)) {}

    explicit constexpr Result(Err<E> value) : m_storage(std::move(value)) {}

    template <typename... Args>
    explicit constexpr Result(ok_tag_t, Args&&... args)
        : m_storage(ok_tag, std::forward<Args>(args)...) {}

    template <typename... Args>
    explicit constexpr Result(err_tag_t, Args&&... args)
        : m_storage(err_tag, std::forward<Args>(args)...) {}

    constexpr Result(const Result& other) noexcept(
            std::is_nothrow_copy_constructible_v<details::ResultStorage<T, E>>) =
            default;

    constexpr Result& operator=(const Result& other) noexcept(
            std::is_nothrow_copy_assignable_v<details::ResultStorage<T, E>>) =
            default;

    constexpr Result(Result&& other) noexcept(
            std::is_nothrow_move_constructible_v<details::ResultStorage<T, E>>) =
            default;

    constexpr Result& operator=(Result&& other) noexcept(
            std::is_nothrow_move_assignable_v<details::ResultStorage<T, E>>) =
            default;

    constexpr Result clone() const { return *this; }

    [[nodiscard]] constexpr bool is_ok() const noexcept {
        return m_storage.kind() == ResultKind::Ok;
    }

    [[nodiscard]] constexpr bool is_err() const noexcept {
        return m_storage.kind() == ResultKind::Err;
    }

    [[nodiscard]] constexpr ResultKind kind() const noexcept {
        return m_storage.kind();
    }

    explicit constexpr operator bool() const noexcept { return is_ok(); }

    constexpr bool operator==(const Ok<T>& other) const noexcept {
        if constexpr(std::is_same_v<T, unit_t>) {
            return true;
        } else {
            return kind() == ResultKind::Ok &&
                    m_storage.template get<T>() == other.value();
        }
    }

    constexpr bool operator!=(const Ok<T>& other) const noexcept {
        return !(*this == other);
    }

    constexpr bool operator==(const Err<E>& other) const noexcept {
        return kind() == ResultKind::Err &&
                m_storage.template get<E>() == other.value();
    }

    constexpr bool operator!=(const Err<E>& other) const noexcept {
        return !(*this == other);
    }

    constexpr bool operator==(const Result& other) const noexcept {
        if(kind() != other.kind()) {
            return false;
        }
        if(kind() == ResultKind::Ok) {
            if constexpr(std::is_same_v<T, unit_t>) {
                return true;
            } else {
                return m_storage.template get<T>() ==
                        other.m_storage.template get<T>();
            }
        }
        return m_storage.template get<E>() == other.m_storage.template get<E>();
    }

    constexpr bool operator!=(const Result& other) const noexcept {
        return !(*this == other);
    }

    // ===== Accessors ===== {{{

    constexpr optional<std::reference_wrapper<const T>> ok() const& {
        if(is_ok()) {
            return std::cref(ok_unchecked());
        }
        return nullopt;
    }

    constexpr optional<std::reference_wrapper<T>> ok() & {
        if(is_ok()) {
            return std::ref(ok_unchecked());
        }
        return nullopt;
    }

    constexpr optional<T> ok() && {
        if(is_ok()) {
            return ok_unchecked();
        }
        return nullopt;
    }

    constexpr optional<std::reference_wrapper<const E>> err() const& {
        if(is_err()) {
            return std::cref(err_unchecked());
        }
        return nullopt;
    }

    constexpr optional<std::reference_wrapper<E>> err() & {
        if(is_err()) {
            return std::ref(err_unchecked());
        }
        return nullopt;
    }

    constexpr optional<E> err() && {
        if(is_err()) {
            return err_unchecked();
        }
        return nullopt;
    }

    constexpr const E& try_err() const {
        if(!is_err()) {
            details::terminate("Called `try_err` on an Ok value");
        }
        return err_unchecked();
    }

    constexpr E& try_err() {
        if(!is_err()) {
            details::terminate("Called `try_err` on an Ok value");
        }
        return err_unchecked();
    }

    constexpr const T& try_ok() const {
        if(!is_ok()) {
            details::terminate("Called `try_ok` on an Err value");
        }
        return ok_unchecked();
    }

    constexpr T& try_ok() {
        if(!is_ok()) {
            details::terminate("Called `try_ok` on an Err value");
        }
        return ok_unchecked();
    }

    constexpr T&& unwrap() {
        if(!is_ok()) {
            details::terminate("Called `unwrap` on an Err value");
        }
        return std::move(*this).ok_unchecked();
    }

    constexpr T&& unwrap_or(T&& value) {
        if(!is_ok()) {
            return value;
        }
        return std::move(*this).ok_unchecked();
    }

    constexpr T&& unwrap_or_default() {
        static_assert(std::is_default_constructible_v<T>,
                "`unwrap_or_default` requires T to be default constructible");
        if(!is_ok()) {
            return T();
        }
        return std::move(*this).ok_unchecked();
    }

    constexpr E&& unwrap_err() {
        if(!is_err()) {
            details::terminate("Called `unwrap_err` on an Err value");
        }
        return std::move(*this).err_unchecked();
    }

    constexpr E&& unwrap_err_or(E&& error) {
        if(!is_err()) {
            return error;
        }
        return std::move(*this).err_unchecked();
    }

    constexpr E&& unwrap_err_or_default() {
        static_assert(std::is_default_constructible_v<T>,
                "`unwrap_err_or_default` requires E to be default "
                "constructible");
        if(!is_err()) {
            return E();
        }
        return std::move(*this).err_unchecked();
    }

    constexpr T&& expect(const std::string_view& message) {
        if(!is_ok()) {
            details::terminate(message);
        }
        return std::move(*this).ok_unchecked();
    }

    constexpr E&& expect_err(const std::string_view& message) {
        if(!is_err()) {
            details::terminate(message);
        }
        return std::move(*this).err_unchecked();
    }

    // }}}
    // ===== Unsafe accessors ===== {{{

    constexpr const T& ok_unchecked() const& noexcept {
        return m_storage.template get<T>();
    }

    constexpr const E& err_unchecked() const& noexcept {
        return m_storage.template get<E>();
    }

    constexpr T& ok_unchecked() & noexcept {
        return m_storage.template get<T>();
    }

    constexpr E& err_unchecked() & noexcept {
        return m_storage.template get<E>();
    }

    constexpr T&& ok_unchecked() && noexcept {
        return std::move(m_storage).template get<T>();
    }

    constexpr E&& err_unchecked() && noexcept {
        return std::move(m_storage).template get<E>();
    }

    // }}}
    // ===== Combinators and adapters ===== {{{
    template <typename F,
            typename T2 = std::invoke_result_t<F, T>,
            std::enable_if_t<std::is_invocable_r_v<T2, F, T>, int> = 0>
    Result<T2, E> map(F&& map_fn) const {
        if(is_ok()) {
            return Result<T2, E>(Ok(map_fn(std::move(*this).ok_unchecked())));
        }
        return Result<T2, E>(Err(std::move(*this).err_unchecked()));
    }

    template <typename F,
            typename E2 = std::invoke_result_t<F, E>,
            std::enable_if_t<std::is_invocable_r_v<E2, F, E>, int> = 0>
    Result<T, E2> map_err(F&& map_fn) {
        if(is_ok()) {
            return Result<T, E2>(Ok(std::move(*this).ok_unchecked()));
        }
        return Result<T, E2>(Err(map_fn(std::move(*this).err_unchecked())));
    }

    template <typename T2>
    Result<T2, E> and_(Result<T2, E> other) {
        if(is_ok()) {
            return other;
        }
        return Result<T2, E>(Err(std::move(*this).err_unchecked()));
    }

    template <typename F,
            typename T2 = typename std::invoke_result_t<F, T>::value_type,
            std::enable_if_t<std::is_invocable_r_v<Result<T2, E>, F, T>, int> =
                    0>
    Result<T2, E> and_then(F&& fn) {
        if(is_ok()) {
            return fn(std::move(*this).ok_unchecked());
        }
        return Result<T2, E>(Err(std::move(*this).err_unchecked()));
    }

    template <typename E2>
    Result<T, E2> or_(Result<T, E2> other) {
        if(is_err()) {
            return other;
        }
        return Result<T, E2>(Ok(std::move(*this).ok_unchecked()));
    }

    template <typename F,
            typename E2 = typename std::invoke_result_t<F, E>::error_type,
            std::enable_if_t<std::is_invocable_r_v<Result<T, E2>, F, E>, int> =
                    0>
    Result<T, E2> or_else(F&& fn) {
        if(is_err()) {
            return fn(std::move(*this).err_unchecked());
        }
        return Result<T, E2>(Ok(std::move(*this).ok_unchecked()));
    }

    // }}}

private:
    details::ResultStorage<T, E> m_storage;
};

template <typename T, typename T2, typename E>
constexpr bool operator<(const Result<T, E>& lhs, const Result<T2, E>& rhs) {
    if(lhs.is_err() && rhs.is_err()) {
        return false;
    }
    if(lhs.is_err()) {
        return true;
    }
    return lhs.try_ok() < rhs.try_ok();
}

template <typename T, typename T2, typename E>
constexpr bool operator<=(const Result<T, E>& lhs, const Result<T2, E>& rhs) {
    if(lhs.is_err() && rhs.is_err()) {
        return true;
    }
    if(lhs.is_err()) {
        return true;
    }
    return lhs.try_ok() <= rhs.try_ok();
}

template <typename T, typename T2, typename E>
constexpr bool operator>(const Result<T, E>& lhs, const Result<T2, E>& rhs) {
    return !(lhs <= rhs);
}

template <typename T, typename T2, typename E>
constexpr bool operator>=(const Result<T, E>& lhs, const Result<T2, E>& rhs) {
    return !(lhs < rhs);
}

template <typename T, typename T2, typename E>
constexpr bool operator<(const Result<T, E>& lhs, Ok<T2> rhs) {
    return lhs < Result<T2, E>(std::move(rhs));
}

template <typename T, typename T2, typename E>
constexpr bool operator<=(const Result<T, E>& lhs, Ok<T2> rhs) {
    return lhs <= Result<T2, E>(std::move(rhs));
}

template <typename T, typename T2, typename E>
constexpr bool operator>(const Result<T, E>& lhs, Ok<T2> rhs) {
    return lhs > Result<T2, E>(std::move(rhs));
}

template <typename T, typename T2, typename E>
constexpr bool operator>=(const Result<T, E>& lhs, Ok<T2> rhs) {
    return lhs >= Result<T2, E>(std::move(rhs));
}

template <typename T, typename E>
constexpr bool operator<(const Result<T, E>& lhs, Err<E> rhs) {
    return lhs < Result<T, E>(std::move(rhs));
}

template <typename T, typename E>
constexpr bool operator<=(const Result<T, E>& lhs, Err<E> rhs) {
    return lhs <= Result<T, E>(std::move(rhs));
}

template <typename T, typename E>
constexpr bool operator>(const Result<T, E>& lhs, Err<E> rhs) {
    return lhs > Result<T, E>(std::move(rhs));
}

template <typename T, typename E>
constexpr bool operator>=(const Result<T, E>& lhs, Err<E> rhs) {
    return lhs >= Result<T, E>(std::move(rhs));
}

template <typename T>
std::ostream& operator<<(std::ostream& stream, unit_t) {
    stream << "()";
    return stream;
}

template <typename T>
std::ostream& operator<<(std::ostream& stream, const Ok<T>& ok) {
    if constexpr(std::is_same_v<T, unit_t>) {
        stream << "Ok()" << std::endl;
    } else {
        stream << "Ok(" << ok.value() << ")";
    }
    return stream;
}

template <typename T>
std::ostream& operator<<(std::ostream& stream, const Err<T>& err) {
    stream << "Err(" << err.value() << ")";
    return stream;
}

template <typename T, typename E>
std::ostream& operator<<(std::ostream& stream, const Result<T, E>& result) {
    switch(result.kind()) {
    case ResultKind::Ok: {
        if constexpr(std::is_same_v<T, unit_t>) {
            stream << "Ok()";
        } else {
            stream << "Ok(" << result.ok().value() << ")";
        }
        break;
    }
    case ResultKind::Err: {
        stream << "Err( " << result.err().value() << ")";
        break;
    }
    default:
        stream << "INVALID RESULT";
        break;
    }
    return stream;
}

template <typename T>
std::ostream& operator<<(
        std::ostream& stream, const std::reference_wrapper<T>& obj) {
    stream << obj.get();
    return stream;
}
} // namespace result

namespace std {
template <typename T, typename E>
struct hash<result::Result<T, E>> {
    std::size_t operator()(const result::Result<T, E>& result) const noexcept {
        if(result.is_ok()) {
            return hash<T>()(result.ok_unchecked());
        }
        return hash<E>()(result.err_unchecked());
    }
};
} // namespace std

#define PROPAGATE(res)                                                         \
    {                                                                          \
        auto& r = res;                                                         \
        if(r.is_err()) {                                                       \
            return r;                                                          \
        }                                                                      \
    }

#endif // CPP_RESULT_HPP
