object TSPData {

  type City = Int
  case class Pt(x: Int, y: Int)

  // a280 dataset from TSPLIB - http://www.iwr.uni-heidelberg.de/groups/comopt/software/TSPLIB95/
  // By way of Rich Hickey's tsp-data.clj, in which he "twiddled one dupe node"
  val coords = Map(
    (  0 -> Pt(288,149)), ( 32 -> Pt( 90,165)), ( 64 -> Pt( 32, 89)), ( 96 -> Pt( 24, 17)), (128 -> Pt(132,137)), (160 -> Pt(104, 49)), (192 -> Pt(156, 25)), (224 -> Pt(260, 37)), (256 -> Pt(228,101)),
    (  1 -> Pt(288,129)), ( 33 -> Pt( 80,157)), ( 65 -> Pt( 24, 89)), ( 97 -> Pt( 32, 17)), (129 -> Pt(140,137)), (161 -> Pt(104, 41)), (193 -> Pt(172, 21)), (225 -> Pt(260, 45)), (257 -> Pt(228,109)),
    (  2 -> Pt(270,133)), ( 34 -> Pt( 64,157)), ( 66 -> Pt( 16, 97)), ( 98 -> Pt( 44, 11)), (130 -> Pt(148,137)), (162 -> Pt(104, 33)), (194 -> Pt(180, 21)), (226 -> Pt(260, 53)), (258 -> Pt(228,117)),
    (  3 -> Pt(256,141)), ( 35 -> Pt( 64,165)), ( 67 -> Pt( 16,109)), ( 99 -> Pt( 56,  9)), (131 -> Pt(156,137)), (163 -> Pt(104, 25)), (195 -> Pt(180, 29)), (227 -> Pt(260, 61)), (259 -> Pt(228,125)),
    (  4 -> Pt(256,157)), ( 36 -> Pt( 56,169)), ( 68 -> Pt(  8,109)), (100 -> Pt( 56, 17)), (132 -> Pt(164,137)), (164 -> Pt(104, 17)), (196 -> Pt(172, 29)), (228 -> Pt(260, 69)), (260 -> Pt(220,125)),
    (  5 -> Pt(246,157)), ( 37 -> Pt( 56,161)), ( 69 -> Pt(  8, 97)), (101 -> Pt( 56, 25)), (133 -> Pt(172,125)), (165 -> Pt( 92,  9)), (197 -> Pt(172, 37)), (229 -> Pt(260, 77)), (261 -> Pt(212,117)),
    (  6 -> Pt(236,169)), ( 38 -> Pt( 56,153)), ( 70 -> Pt(  8, 89)), (102 -> Pt( 56, 33)), (134 -> Pt(172,117)), (166 -> Pt( 80,  9)), (198 -> Pt(172, 45)), (230 -> Pt(276, 77)), (262 -> Pt(204,109)),
    (  7 -> Pt(228,169)), ( 39 -> Pt( 56,145)), ( 71 -> Pt(  8, 81)), (103 -> Pt( 56, 41)), (135 -> Pt(172,109)), (167 -> Pt( 72,  9)), (199 -> Pt(180, 45)), (231 -> Pt(276, 69)), (263 -> Pt(196,101)),
    (  8 -> Pt(228,161)), ( 40 -> Pt( 56,137)), ( 72 -> Pt(  8, 73)), (104 -> Pt( 64, 41)), (136 -> Pt(172,101)), (168 -> Pt( 64, 21)), (200 -> Pt(180, 37)), (232 -> Pt(276, 61)), (264 -> Pt(188, 93)),
    (  9 -> Pt(220,169)), ( 41 -> Pt( 56,129)), ( 73 -> Pt(  8, 65)), (105 -> Pt( 72, 41)), (137 -> Pt(172, 93)), (169 -> Pt( 72, 25)), (201 -> Pt(188, 41)), (233 -> Pt(276, 53)), (265 -> Pt(180, 93)),
    ( 10 -> Pt(212,169)), ( 42 -> Pt( 56,121)), ( 74 -> Pt(  8, 57)), (106 -> Pt( 72, 49)), (138 -> Pt(172, 85)), (170 -> Pt( 80, 26)), (202 -> Pt(196, 49)), (234 -> Pt(284, 53)), (266 -> Pt(180,101)),
    ( 11 -> Pt(204,169)), ( 43 -> Pt( 40,121)), ( 75 -> Pt( 16, 57)), (107 -> Pt( 56, 49)), (139 -> Pt(180, 85)), (171 -> Pt( 80, 25)), (203 -> Pt(204, 57)), (235 -> Pt(284, 61)), (267 -> Pt(180,109)),
    ( 12 -> Pt(196,169)), ( 44 -> Pt( 40,129)), ( 76 -> Pt(  8, 49)), (108 -> Pt( 48, 51)), (140 -> Pt(180, 77)), (172 -> Pt( 80, 41)), (204 -> Pt(212, 65)), (236 -> Pt(284, 69)), (268 -> Pt(180,117)),
    ( 13 -> Pt(188,169)), ( 45 -> Pt( 40,137)), ( 77 -> Pt(  8, 41)), (109 -> Pt( 56, 57)), (141 -> Pt(180, 69)), (173 -> Pt( 88, 49)), (205 -> Pt(220, 73)), (237 -> Pt(284, 77)), (269 -> Pt(180,125)),
    ( 14 -> Pt(196,161)), ( 46 -> Pt( 40,145)), ( 78 -> Pt( 24, 45)), (110 -> Pt( 56, 65)), (142 -> Pt(180, 61)), (174 -> Pt(104, 57)), (206 -> Pt(228, 69)), (238 -> Pt(284, 85)), (270 -> Pt(196,145)),
    ( 15 -> Pt(188,145)), ( 47 -> Pt( 40,153)), ( 79 -> Pt( 32, 41)), (111 -> Pt( 48, 63)), (143 -> Pt(180, 53)), (175 -> Pt(124, 69)), (207 -> Pt(228, 77)), (239 -> Pt(284, 93)), (271 -> Pt(204,145)),
    ( 16 -> Pt(172,145)), ( 48 -> Pt( 40,161)), ( 80 -> Pt( 32, 49)), (112 -> Pt( 48, 73)), (144 -> Pt(172, 53)), (176 -> Pt(124, 77)), (208 -> Pt(236, 77)), (240 -> Pt(284,101)), (272 -> Pt(212,145)),
    ( 17 -> Pt(164,145)), ( 49 -> Pt( 40,169)), ( 81 -> Pt( 32, 57)), (113 -> Pt( 56, 73)), (145 -> Pt(172, 61)), (177 -> Pt(132, 81)), (209 -> Pt(236, 69)), (241 -> Pt(288,109)), (273 -> Pt(220,145)),
    ( 18 -> Pt(156,145)), ( 50 -> Pt( 32,169)), ( 82 -> Pt( 32, 65)), (114 -> Pt( 56, 81)), (146 -> Pt(172, 69)), (178 -> Pt(140, 65)), (210 -> Pt(236, 61)), (242 -> Pt(280,109)), (274 -> Pt(228,145)),
    ( 19 -> Pt(148,145)), ( 51 -> Pt( 32,161)), ( 83 -> Pt( 32, 73)), (115 -> Pt( 48, 83)), (147 -> Pt(172, 77)), (179 -> Pt(132, 61)), (211 -> Pt(228, 61)), (243 -> Pt(276,101)), (275 -> Pt(236,145)),
    ( 20 -> Pt(140,145)), ( 52 -> Pt( 32,153)), ( 84 -> Pt( 32, 81)), (116 -> Pt( 56, 89)), (148 -> Pt(164, 81)), (180 -> Pt(124, 61)), (212 -> Pt(228, 53)), (244 -> Pt(276, 93)), (276 -> Pt(246,141)),
    ( 21 -> Pt(148,169)), ( 53 -> Pt( 32,145)), ( 85 -> Pt( 40, 83)), (117 -> Pt( 56, 97)), (149 -> Pt(148, 85)), (181 -> Pt(124, 53)), (213 -> Pt(236, 53)), (245 -> Pt(276, 85)), (277 -> Pt(252,125)),
    ( 22 -> Pt(164,169)), ( 54 -> Pt( 32,137)), ( 86 -> Pt( 40, 73)), (118 -> Pt(104, 97)), (150 -> Pt(124, 85)), (182 -> Pt(124, 45)), (214 -> Pt(236, 45)), (246 -> Pt(268, 97)), (278 -> Pt(260,129)),
    ( 23 -> Pt(172,169)), ( 55 -> Pt( 32,129)), ( 87 -> Pt( 40, 63)), (119 -> Pt(104,105)), (151 -> Pt(124, 93)), (183 -> Pt(124, 37)), (215 -> Pt(228, 45)), (247 -> Pt(260,109)), (279 -> Pt(280,133)),
    ( 24 -> Pt(156,169)), ( 56 -> Pt( 32,121)), ( 88 -> Pt( 40, 51)), (120 -> Pt(104,113)), (152 -> Pt(124,109)), (184 -> Pt(124, 29)), (216 -> Pt(228, 37)), (248 -> Pt(252,101)), ( 25 -> Pt(140,169)),
    ( 57 -> Pt( 32,113)), ( 89 -> Pt( 44, 43)), (121 -> Pt(104,121)), (153 -> Pt(124,125)), (185 -> Pt(132, 21)), (217 -> Pt(236, 37)), (249 -> Pt(260, 93)), ( 26 -> Pt(132,169)), ( 58 -> Pt( 40,113)),
    ( 90 -> Pt( 44, 35)), (122 -> Pt(104,129)), (154 -> Pt(124,117)), (186 -> Pt(124, 21)), (218 -> Pt(236, 29)), (250 -> Pt(260, 85)), ( 27 -> Pt(124,169)), ( 59 -> Pt( 56,113)), ( 91 -> Pt( 44, 27)),
    (123 -> Pt(104,137)), (155 -> Pt(124,101)), (187 -> Pt(120,  9)), (219 -> Pt(228, 29)), (251 -> Pt(236, 85)), ( 28 -> Pt(116,161)), ( 60 -> Pt( 56,105)), ( 92 -> Pt( 32, 25)), (124 -> Pt(104,145)),
    (156 -> Pt(104, 89)), (188 -> Pt(128,  9)), (220 -> Pt(228, 21)), (252 -> Pt(228, 85)), ( 29 -> Pt(104,153)), ( 61 -> Pt( 48, 99)), ( 93 -> Pt( 24, 25)), (125 -> Pt(116,145)), (157 -> Pt(104, 81)),
    (189 -> Pt(136,  9)), (221 -> Pt(236, 21)), (253 -> Pt(228, 93)), ( 30 -> Pt(104,161)), ( 62 -> Pt( 40, 99)), ( 94 -> Pt( 16, 25)), (126 -> Pt(124,145)), (158 -> Pt(104, 73)), (190 -> Pt(148,  9)),
    (222 -> Pt(252, 21)), (254 -> Pt(236, 93)), ( 31 -> Pt(104,169)), ( 63 -> Pt( 32, 97)), ( 95 -> Pt( 16, 17)), (127 -> Pt(132,145)), (159 -> Pt(104, 65)), (191 -> Pt(162,  9)), (223 -> Pt(260, 29)),
    (255 -> Pt(236,101)))

  val optimalTour = Array(
    0, 1, 241, 242, 243, 240, 239, 238, 237, 236, 235, 234, 233, 232, 231, 230,
    245, 244, 246, 249, 250, 229, 228, 227, 226, 225, 224, 223, 222, 221, 220,
    219, 218, 217, 216, 215, 214, 213, 212, 211, 210, 209, 206, 205, 204, 203,
    202, 201, 200, 197, 196, 195, 194, 193, 192, 191, 190, 189, 188, 187, 186,
    185, 184, 183, 182, 181, 180, 175, 179, 178, 149, 177, 176, 150, 151, 155,
    152, 154, 153, 128, 129, 130, 19, 20, 127, 126, 125, 124, 123, 122, 121,
    120, 119, 118, 156, 157, 158, 159, 174, 160, 161, 162, 163, 164, 165, 166,
    167, 168, 169, 171, 170, 172, 173, 106, 105, 104, 103, 102, 101, 100, 99,
    98, 97, 96, 95, 94, 93, 92, 91, 90, 89, 88, 108, 107, 109, 110, 111, 87,
    86, 112, 113, 114, 116, 115, 85, 84, 83, 82, 81, 80, 79, 78, 77, 76, 75,
    74, 73, 72, 71, 70, 69, 68, 67, 66, 65, 64, 63, 57, 56, 55, 54, 53, 52, 51,
    50, 49, 48, 47, 46, 45, 44, 43, 58, 62, 61, 117, 60, 59, 42, 41, 40, 39,
    38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 21, 24, 22, 23, 13,
    14, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 276, 275, 274, 273, 272, 271, 270, 15,
    16, 17, 18, 131, 132, 133, 269, 268, 134, 135, 267, 266, 136, 137, 138,
    148, 147, 146, 145, 144, 198, 199, 143, 142, 141, 140, 139, 265, 264, 263,
    262, 261, 260, 259, 258, 257, 256, 253, 252, 207, 208, 251, 254, 255, 248,
    247, 277, 278, 2, 279, 0)
}