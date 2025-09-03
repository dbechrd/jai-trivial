Vector2i :: struct { x: s64; y: s64; };
operator == :: (a: Vector2i, b: Vector2i) -> bool {
    return a.x == b.x && a.y == b.y;
}
operator + :: (a: Vector2i, b: Vector2i) -> Vector2i {
    return .{a.x+b.x, a.y+b.y};
}
operator - :: (a: Vector2i, b: Vector2i) -> Vector2i {
    return .{a.x-b.x, a.y-b.y};
}
operator * :: (a: Vector2i, b: Vector2i) -> Vector2i {
    return .{a.x*b.x, a.y*b.y};
}

make_vector2i :: (x: s64, y: s64) -> Vector2i {
    return .{ x, y };
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

ceil :: (v: Vector2) -> Vector2 {
    return .{ ceil(v.x), ceil(v.y) };
}

round :: (f: float) -> float {
    return floor(f + 0.5);
}

round :: (v: Vector2) -> Vector2 {
    return .{ floor(v.x + 0.5), floor(v.y + 0.5) };
}

move_toward :: (f: *float, target: float, dt: float, rate_up: float, rate_down := -1.0) {
    value := f.*;

    if value > target {
        if rate_down == -1 rate_down = rate_up;
        value -= dt * rate_down;
        if value < target then value = target;
    } else if value < target {
        value += dt * rate_up;
        if value > target then value = target;
    }

    f.* = value;
}

move_toward_smooth :: (f: *float, target: float, dt: float, rate: float) {
    delta := abs(target - f.*) * rate;
    move_toward(f, target, dt, delta);
}

move_toward_smooth :: (v: *Vector2, target: Vector2, dt: float, rate: float) {
    dx := abs(target.x - v.x) * rate;
    dy := abs(target.y - v.y) * rate;
    move_toward(*v.x, target.x, dt, dx);
    move_toward(*v.y, target.y, dt, dy);
}

letterbox :: (size: Vector2, box: Vector2, power_of_two := false) -> boxed_size: Vector2, scale: float {
    scale := 1.0;
    scale = min(scale, box.x / size.x);
    scale = min(scale, box.y / size.y);
    if power_of_two {
        pow := 1.0;
        while pow > scale {
            pow *= 0.5;
        }
        scale = pow;
    }

    boxed_size := size;
    if scale < 1.0 {
        boxed_size.x = floor(size.x * scale);
        boxed_size.y = floor(size.y * scale);
    }
    return boxed_size, scale;
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

append :: (builder: *String_Builder, value: int) {
    print_integer(builder, .{value=value});
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

log :: (str: string, loc := #caller_location, flags := Log_Flags.NONE, user_flags : u32 = 0, section : *Log_Section = null) {
    builder: String_Builder;

    print_to_builder(*builder, str);

    s := builder_to_string(*builder,, allocator = __temporary_allocator);
    if !s.count return;

    info: Log_Info;
    info.source_identifier = context.log_source_identifier;
    info.location          = loc;
    info.common_flags      = flags;
    info.user_flags        = user_flags;
    info.section           = section;

    context.logger(s, context.logger_data, info);
}

log_warning :: (format_string: string, args: .. Any, loc := #caller_location, flags := Log_Flags.NONE, user_flags : u32 = 0, section : *Log_Section = null) {
    new_flags := flags | .WARNING;
    log(format_string, ..args, loc, new_flags, user_flags, section);
} @PrintLike

enum_to_string :: (value: $T) -> string {
    info_enum := type_info(T);
    for info_enum.values if it == xx value {
        return info_enum.names[it_index];
    }
    return "";
}

string_to_enum :: (name: string, $T: Type) -> success: bool, result: T {
    info_enum := type_info(T);
    for info_enum.names if it == name {
        return true, xx info_enum.values[it_index];
    }
    return false, -1;
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