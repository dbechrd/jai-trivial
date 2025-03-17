Vector2i :: struct { x: s64; y: s64; };
operator == :: (a: Vector2i, b: Vector2i) -> bool {
    return a.x == b.x && a.y == b.y;
}
operator + :: (a: Vector2i, b: Vector2i) -> Vector2i {
    return .{a.x+b.x, a.y+b.y};
}
operator * :: (a: Vector2i, b: Vector2i) -> Vector2i {
    return .{a.x*b.x, a.y*b.y};
}

make_vector2i :: (v: Vector2) -> Vector2i {
    return .{ xx floor(v.x), xx floor(v.y) };
}

make_vector2 :: (v: Vector2i) -> Vector2 {
    return .{ xx v.x, xx v.y };
}

floor :: (v: Vector2) -> Vector2 {
    return .{ floor(v.x), floor(v.y) };
}

array_fill_byte :: (array: [] $T, value: u8) {
    memset(array.data, value, size_of(T) * array.count);
}

array_zero :: (array: [] $T) {
    memset(array.data, 0, size_of(T) * array.count);
}

array_equal :: (a: [$N] $T, b: [N] T) -> bool {
    return !memcmp(a.data, b.data, a.count);
}

array_copy :: (a: [$N] $T, b: [N] T) {
    memcpy(a.data, b.data, N * size_of(T));
}

array_reverse :: (array: [] $T) {
    for 0..array.count/2-1 {
        Swap(*(array[it]), *(array[array.count-1-it]));
    }
}

pointer_diff :: (a: *$T, b: *$U) -> s64 #expand {
    return cast(s64)((cast(*u8) a) - (cast(*u8) b));
}

hexval :: (c: u8) -> int {
    if c == {
        case #char "0"; return 0;
        case #char "1"; return 1;
        case #char "2"; return 2;
        case #char "3"; return 3;
        case #char "4"; return 4;
        case #char "5"; return 5;
        case #char "6"; return 6;
        case #char "7"; return 7;
        case #char "8"; return 8;
        case #char "9"; return 9;
        case #char "A"; #through; case #char "a"; return 10;
        case #char "B"; #through; case #char "b"; return 11;
        case #char "C"; #through; case #char "c"; return 12;
        case #char "D"; #through; case #char "d"; return 13;
        case #char "E"; #through; case #char "e"; return 14;
        case #char "F"; #through; case #char "f"; return 15;
    }
    return -1;
}

get_identifier_name :: (name: Code) -> string {
    #import "Compiler";

    node := compiler_get_nodes(name);
    if node.kind != .IDENT {
        compiler_report("Not an identifier.", make_location(node));
        return "";
    }

    ident := cast(*Code_Ident) node;
    return ident.name;
}

#scope_file

#import "Basic";
#import "Math";