static const char *colors[SchemeLast][2] = {{
    /*     fg         bg       */
    [SchemeNorm] = {{"{foreground}", "{background}"}},
    [SchemeSel] = {{"{foreground}", "{color8}"}},
    [SchemeSelHighlight] = {{"{color4}", "{color8}"}},
    [SchemeNormHighlight] = {{"{color4}", "{background}"}},
    [SchemeOut] = {{"{foreground}", "{color14}"}},
}};
