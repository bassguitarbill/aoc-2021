testData =  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
            \\n\
            \22 13 17 11  0\n\
            \ 8  2 23  4 24\n\
            \21  9 14 16  7\n\
            \ 6 10  3 18  5\n\
            \ 1 12 20 15 19\n\
            \\n\
            \ 3 15  0  2 22\n\
            \ 9 18 13 17  5\n\
            \19  8  7 25 23\n\
            \20 11 10 24  4\n\
            \14 21 16 12  6\n\
            \\n\
            \14 21 17 24  4\n\
            \10 16 15  9 19\n\
            \18  8 23 26 20\n\
            \22 11 13  6  5\n\
            \ 2  0 12  3  7"

realData =  "15,61,32,33,87,17,56,73,27,83,0,18,43,8,86,37,40,6,93,25,14,68,64,57,39,46,55,13,21,72,51,81,78,79,52,65,36,92,97,28,9,24,22,69,70,42,3,4,63,50,91,16,41,94,77,85,49,12,76,67,11,62,99,54,95,1,74,34,88,89,82,48,84,98,58,44,5,53,7,19,29,30,35,26,31,10,60,59,80,71,45,38,20,66,47,2,23,96,90,75\n\
            \\n\
            \26 68  3 95 59\n\
            \40 88 50 22 48\n\
            \75 67  8 64  6\n\
            \29  2 73 78  5\n\
            \49 25 80 89 96\n\
            \\n\
            \57 26 21 56 70\n\
            \38 48 78 40 54\n\
            \82 71 22 24  4\n\
            \16  9 65 42 79\n\
            \43 94 39 12 67\n\
            \\n\
            \70 97 26 39 22\n\
            \65 11 69  6 93\n\
            \71 74 72 57 59\n\
            \61 21 89 86 17\n\
            \66 15 94 79 85\n\
            \\n\
            \46  4 55  2 92\n\
            \ 7  8 53 65 42\n\
            \49 35 99 77  0\n\
            \82 28 25 43 33\n\
            \79 12 58 81 71\n\
            \\n\
            \82  5 63 98 48\n\
            \78 61 29 68  9\n\
            \19  6 69 73 89\n\
            \20 81  8 17 11\n\
            \24 90 59 21 91\n\
            \\n\
            \76 87 99 12 78\n\
            \14 97 19 70  9\n\
            \66 44 88 30 63\n\
            \58 85 55 24 36\n\
            \49 10  6  2 92\n\
            \\n\
            \33 63 13 28 70\n\
            \73 67 69 27 91\n\
            \85 71 98 37 36\n\
            \46 80 21 97 75\n\
            \54 92 44 11  2\n\
            \\n\
            \88  4 55 76 75\n\
            \29 79 30 26 18\n\
            \45 31 72 77 86\n\
            \ 7 14 74 94 98\n\
            \93 78  8 66 16\n\
            \\n\
            \75 34 74 94 59\n\
            \31 41 11 85 68\n\
            \12 24 19 80 29\n\
            \ 7 97 77 73 14\n\
            \56 27 26 72 35\n\
            \\n\
            \15 35 65 77 76\n\
            \31  9 89 73 63\n\
            \54 18  4 39 12\n\
            \22 75 30 67 33\n\
            \79 84  6 64 92\n\
            \\n\
            \75 58 28 83 37\n\
            \81 82 62  7 25\n\
            \31 55 51 42 41\n\
            \53 38 85 69 63\n\
            \ 0 89 20 96 79\n\
            \\n\
            \43 29 62 59 27\n\
            \ 6  1 66 77 32\n\
            \70 37 57 82 38\n\
            \22 60 19 68 88\n\
            \99 93 40 28 47\n\
            \\n\
            \76 47 68 60 28\n\
            \57 34 35 83 11\n\
            \ 2 51  0 49  9\n\
            \31 65 14 90 46\n\
            \96 81 32 63 24\n\
            \\n\
            \17 38 59 27 54\n\
            \26 78 91 40 80\n\
            \74 98 77 14 53\n\
            \13 49  3 63 46\n\
            \45 34 64 66 37\n\
            \\n\
            \ 9 62 22 36 32\n\
            \40 56  2 98 74\n\
            \71 45 58 31 42\n\
            \14  1 51 39 66\n\
            \86 18 90 88 20\n\
            \\n\
            \98 61  0 78 22\n\
            \ 6 42 83 74 30\n\
            \17 39 45 52 75\n\
            \49 18  4 67 91\n\
            \12 84 89 10 87\n\
            \\n\
            \12 40 69 11 87\n\
            \70 73 14 92 78\n\
            \99 67 34 50 94\n\
            \43 49 82 68  1\n\
            \86 20 16 18 63\n\
            \\n\
            \26 13 68 33 88\n\
            \41 73 17 78 12\n\
            \10 82 62 19  8\n\
            \58  3 84  5 70\n\
            \97 91  2 46 15\n\
            \\n\
            \48 61 90 54 73\n\
            \46 77 47 37 49\n\
            \41 81 79 88  0\n\
            \23 86 84 65 60\n\
            \87 91 34 78 38\n\
            \\n\
            \ 4  9 58 62 99\n\
            \35 11  6 13 25\n\
            \68 67 78 28 19\n\
            \95 44 63 92 79\n\
            \20 61 54 88 81\n\
            \\n\
            \78 75 59 79 44\n\
            \ 6  3  7 33 10\n\
            \41  2 90  9 81\n\
            \20  4 55 54 27\n\
            \28 51 64 95 21\n\
            \\n\
            \28 55 99 76 58\n\
            \61 18 71 67 65\n\
            \80 84  6 63 46\n\
            \72 81 29 35 10\n\
            \64 94 15 26 79\n\
            \\n\
            \57 18 32 13 70\n\
            \81  3 84 91 75\n\
            \10 15 63 16 87\n\
            \97 86 95 58 50\n\
            \83  2  0 12 89\n\
            \\n\
            \48 37  6 61 43\n\
            \49 89 80 93 50\n\
            \64 12 23 82 99\n\
            \38 36 69 67 63\n\
            \25 66 72 92 95\n\
            \\n\
            \ 4 20 11 85 27\n\
            \88 29 59 79 48\n\
            \16  2 64 55 90\n\
            \73 82  1 56 61\n\
            \34  8 23 32  6\n\
            \\n\
            \24 12 80 18 14\n\
            \54 86 89 66 38\n\
            \ 0 62 46 72 17\n\
            \77 37 74 79 85\n\
            \ 1 29 87 42 15\n\
            \\n\
            \54 88 75 16 33\n\
            \76  9 19 69  2\n\
            \14 98 46 87 67\n\
            \32  6 62 45 25\n\
            \30 38 52  0 51\n\
            \\n\
            \15 32 85 22 81\n\
            \49 59 72 74 23\n\
            \54 33 34 84 89\n\
            \37 12 55 83  2\n\
            \11 25 51 24 38\n\
            \\n\
            \72 20 87 38 24\n\
            \74 28 76 42 94\n\
            \23 50 75 80 18\n\
            \15 53 85 95 68\n\
            \88  0 34 59 27\n\
            \\n\
            \15 69  3 45 94\n\
            \ 8 90 27 95 48\n\
            \37  9  2 74 59\n\
            \43 46 25 98 83\n\
            \68 73 24 31 72\n\
            \\n\
            \63 65 61  7 57\n\
            \95 51 66 64 17\n\
            \94 93 44 58 59\n\
            \30 90 97 23 10\n\
            \47 92 56 20 70\n\
            \\n\
            \26 30 45 99 31\n\
            \81 41  3 69 83\n\
            \44 89 74 72 65\n\
            \86 20 29 67  4\n\
            \64 84  9 33 18\n\
            \\n\
            \49  7 38 88 94\n\
            \98 54 36 89 23\n\
            \34 51 70 79 61\n\
            \19 15  4 27 73\n\
            \12 13 35 44 95\n\
            \\n\
            \36 49 60 27 53\n\
            \44 81 23 50 73\n\
            \12 31 48 63 61\n\
            \91 32 47  1 87\n\
            \88 96 70 90 10\n\
            \\n\
            \80 13 46 51 61\n\
            \20 12 71 73 54\n\
            \27 86 21 62 37\n\
            \30 25 67 14  6\n\
            \55 78 88 32 57\n\
            \\n\
            \99 71 10 58 52\n\
            \26  5 36 44 86\n\
            \56 46 16 90  3\n\
            \94 89 53 23 31\n\
            \77 14 34 32 54\n\
            \\n\
            \59 60 15  0 45\n\
            \55 78 48 14 33\n\
            \37 32 20 68 19\n\
            \ 8 24 26 52 23\n\
            \82 70 40 21  7\n\
            \\n\
            \61 70 95 96 18\n\
            \26 89 43 33 92\n\
            \50 88 64 54 76\n\
            \78 19 56  7 68\n\
            \29 59 77 71 63\n\
            \\n\
            \48 28 79 68 64\n\
            \61 38 52 78 59\n\
            \31 41 10 39 77\n\
            \34 15 43 98 73\n\
            \17 54 55 75 27\n\
            \\n\
            \55 49  3  5 71\n\
            \85 48 75 95 45\n\
            \88  2 56 29 31\n\
            \83 70 53 52 66\n\
            \74 27 89 50 91\n\
            \\n\
            \32 61 13  1 37\n\
            \ 9 92 35 26 39\n\
            \98 25  7  5 12\n\
            \ 0 99 17 82 27\n\
            \19 42  8 21 30\n\
            \\n\
            \33 77 59 37 42\n\
            \80 64 61 73 79\n\
            \15 41 58 45 93\n\
            \86  2 92 57 83\n\
            \20 24 17 13 23\n\
            \\n\
            \98 57 16 14 99\n\
            \70 43 97 94 52\n\
            \19 89 88 54 17\n\
            \44 41 15 60  4\n\
            \31 71 33 96 68\n\
            \\n\
            \ 9 69 10 79 43\n\
            \84 19  5 48 71\n\
            \40 22 66 89 82\n\
            \36 62  6 76 81\n\
            \51 18 30 93 75\n\
            \\n\
            \96 42 32 35 36\n\
            \28 81 53 87 92\n\
            \51 34 91 80 15\n\
            \23 29 62 98  4\n\
            \25  2 83 41 46\n\
            \\n\
            \55 79 98 90 27\n\
            \33 75 48 38 39\n\
            \21 65 52 63  2\n\
            \85 53  5 88 15\n\
            \50 96 70  3  8\n\
            \\n\
            \22 45 77 61 69\n\
            \15 81 71 59 26\n\
            \74 12 30  2 27\n\
            \25 78 24 70 65\n\
            \99 66 35 16 57\n\
            \\n\
            \44 85 88 14 64\n\
            \67 55 47 98 99\n\
            \87 57 10 84 27\n\
            \42 28 39 81 56\n\
            \46 76  8 75 95\n\
            \\n\
            \96  4 48 28 81\n\
            \67 29  6 30  8\n\
            \20 24 64  7 12\n\
            \16 71 59 19 99\n\
            \82  9  0 62 87\n\
            \\n\
            \70 98 10 97 92\n\
            \ 7 94 67 20 26\n\
            \77 13 69 61 51\n\
            \ 4 71  3 28 91\n\
            \85 11 27 56 54\n\
            \\n\
            \ 3 98 31 47 75\n\
            \34 95  8 27 42\n\
            \74 49 80 79 11\n\
            \15 17 89 85 33\n\
            \55 52 32 36 45\n\
            \\n\
            \ 4 78 93 29 63\n\
            \13 24 40 17 75\n\
            \12 92 48 82 60\n\
            \26 54  8 47 37\n\
            \41 57 36 32 99\n\
            \\n\
            \ 1 27  8 47 41\n\
            \86 20 53 61 96\n\
            \45 21 80 58 64\n\
            \ 5 95 23 13 10\n\
            \81 87 49 24 50\n\
            \\n\
            \27 78 50 18 43\n\
            \23 75 77 38 29\n\
            \71 93 64  5 56\n\
            \34 84 67 52 79\n\
            \90 95 19 46 88\n\
            \\n\
            \83 41 79 67 69\n\
            \60 47  2 43 85\n\
            \12 17 28 89 81\n\
            \16 18 98 35 62\n\
            \ 7 45 25 40 58\n\
            \\n\
            \79 21  7 85 76\n\
            \55  8 14  3 72\n\
            \25 30 62  6 82\n\
            \38 16 32 95 59\n\
            \27 99 33 75 98\n\
            \\n\
            \65 76 69 98 78\n\
            \94 55 31 73 77\n\
            \10 14 79 58 22\n\
            \26 34 16 87 29\n\
            \ 2 24 30 27 91\n\
            \\n\
            \31 42 38 93 88\n\
            \36 68  7 66 59\n\
            \23 71 45 72 94\n\
            \52 81 84 27 41\n\
            \18 49 76 82 70\n\
            \\n\
            \50 58 95 52 35\n\
            \21 46 68 71 59\n\
            \34 84 76 62 57\n\
            \94 41 99 77 55\n\
            \69 48 97 78 73\n\
            \\n\
            \53 86 48  8 26\n\
            \ 3 72 57 27 23\n\
            \99  4 71 21 50\n\
            \39 18 54 41 82\n\
            \ 7 46  1 65 96\n\
            \\n\
            \63 98 33 80 56\n\
            \89 75 15 22 59\n\
            \69 36  0 86 12\n\
            \21 41 55 49 74\n\
            \ 7 90 76  5 44\n\
            \\n\
            \78 40  2 61 76\n\
            \25  5 42 88 35\n\
            \ 1 41 28 71 85\n\
            \ 3 34 22 72 23\n\
            \15 56 67 38 68\n\
            \\n\
            \11 89 28 48 87\n\
            \57 80  1 42 33\n\
            \59 18  7 24 65\n\
            \30 79 12 68 83\n\
            \44 82  2 53 58\n\
            \\n\
            \ 7 14 22 23 29\n\
            \53 37 48 86  3\n\
            \56 25 54 82 43\n\
            \ 0 91  6 17 49\n\
            \33 95 63 94 12\n\
            \\n\
            \86 62  0 47 69\n\
            \80 91 37 15 46\n\
            \50 28 75 83 31\n\
            \65  5 39 23 55\n\
            \88 84 72 70 74\n\
            \\n\
            \36 31 82 32 78\n\
            \30 18 11 29 38\n\
            \55 84  9 33 57\n\
            \16 51 48 77 58\n\
            \73 22 79 85 54\n\
            \\n\
            \70 66 89 40 55\n\
            \75 17 36 88 28\n\
            \22 97 92 43 72\n\
            \25 27  3 18 45\n\
            \13 14 54 12 74\n\
            \\n\
            \11 43 96 92 51\n\
            \ 7 59  2 32 69\n\
            \79  0 46 68 80\n\
            \18 95 88 39 60\n\
            \84 14 58 36 22\n\
            \\n\
            \82 88 64 85 51\n\
            \45 95 50 27 99\n\
            \15 13 21 69  9\n\
            \53 36 79 22 68\n\
            \83  8 92 65 32\n\
            \\n\
            \31 37 89 28 26\n\
            \17 25 99 20  5\n\
            \97 49 21 60 83\n\
            \55 57 16 40  6\n\
            \45 39 33  0 65\n\
            \\n\
            \86 51  0 31  7\n\
            \95 33 85 87 14\n\
            \32 48 91 46 36\n\
            \60 90 88 38 15\n\
            \52 75 40 23 84\n\
            \\n\
            \25 35 57  8 86\n\
            \ 3 59 46 96 13\n\
            \ 0 41 45 76 79\n\
            \97 36 60 26 53\n\
            \33 74 64 66 93\n\
            \\n\
            \59  3 96 84 71\n\
            \39 90 61 77 19\n\
            \92 38  6 32 54\n\
            \12  5 62 86 75\n\
            \43 98 23 82 33\n\
            \\n\
            \ 1 67 51  6 94\n\
            \57 44 53 90  2\n\
            \19 89 80 30 45\n\
            \42 88 62 98 33\n\
            \20 63 78 56 83\n\
            \\n\
            \47 21 70 31 75\n\
            \19 38 91 85 73\n\
            \22 27 54 86 13\n\
            \ 8 49  7 89 37\n\
            \32 25 17 16  0\n\
            \\n\
            \64 77  7 23 83\n\
            \56  2 17 65 60\n\
            \43 98 68 67 18\n\
            \22 96 72 69 86\n\
            \20 26  4 84 16\n\
            \\n\
            \30 31 95 98 48\n\
            \36 11 92 60  5\n\
            \ 0 76 73 27 14\n\
            \50 46 38 53 33\n\
            \12 97 59 61 51\n\
            \\n\
            \93 45 66 91 63\n\
            \80 75 52 55  1\n\
            \31 68 76 24 79\n\
            \15  2 42 70 20\n\
            \89 90 21 25 48\n\
            \\n\
            \36 99 49 83 57\n\
            \24 79 89 91 63\n\
            \58 47 27 74 38\n\
            \90 54 39 40 98\n\
            \ 7  2 77 14 86\n\
            \\n\
            \44 33 12 86  8\n\
            \65 92 74 52 55\n\
            \20  3 78 28 47\n\
            \80 17 11 41 29\n\
            \62 18 39 48  7\n\
            \\n\
            \63 52 87 81 14\n\
            \91 56  4 84 27\n\
            \ 9 24 68 18 47\n\
            \57 44 26  0 37\n\
            \40 75 11 88 20\n\
            \\n\
            \61  4 91 31 79\n\
            \67  7  6 34 95\n\
            \19 23 62 99 50\n\
            \43  1 37 16 74\n\
            \38 94 47 10 25\n\
            \\n\
            \90 51 37  7 16\n\
            \68 61 28 65  1\n\
            \58 80 49 11 23\n\
            \24  8 12  6  4\n\
            \30 75 19 63 53\n\
            \\n\
            \70 90 34  4 97\n\
            \73 26 87 61 88\n\
            \38  2  0 71 28\n\
            \57 69 18 15 60\n\
            \80 39 78 33 36\n\
            \\n\
            \44 10  3 46 31\n\
            \43 57 12 29 92\n\
            \ 0 61 54 23 52\n\
            \ 5 55 27 93 11\n\
            \24 14 30 87 99\n\
            \\n\
            \ 5 20 33 37 97\n\
            \78 83 50 93 65\n\
            \30 59 74 68 27\n\
            \ 4 32 90 16 79\n\
            \52 22 76 45 41\n\
            \\n\
            \29 53  9 20 15\n\
            \17 61 94 52 83\n\
            \43 82 97 14 57\n\
            \18  2 16 95 72\n\
            \30 39 79 65 25\n\
            \\n\
            \22  5 42 15 73\n\
            \32 16 29 36 77\n\
            \ 9 53 98 69 18\n\
            \97 56 79 66 88\n\
            \90 99  3 10 84\n\
            \\n\
            \71 92 19  1 80\n\
            \17 21  4 54 61\n\
            \27 66 20 63 49\n\
            \18 74 11 70 39\n\
            \97 98 64 34 10\n\
            \\n\
            \66 83 73 54 57\n\
            \68 10  8 17 22\n\
            \53 87 71 18 40\n\
            \43  4 65 89 59\n\
            \27 35 47 15 46\n\
            \\n\
            \87 76 88 54  2\n\
            \42 68 47 44 17\n\
            \16 70 10 53 43\n\
            \ 7 78 12 39 83\n\
            \15 65 96 85 24\n\
            \\n\
            \73 28 78  4 98\n\
            \97 56 16 69  6\n\
            \46 90 18 63 81\n\
            \26 95 19 30 31\n\
            \59 32 49 21 13\n\
            \\n\
            \68 48  7 85 12\n\
            \58 95 41 59 78\n\
            \ 1 28 53 51  9\n\
            \10 93 97 91 65\n\
            \61 75 63 23 57\n\
            \\n\
            \26 82 40 53 11\n\
            \ 0 22 68 99 96\n\
            \64 45 74  5 92\n\
            \84 33 13 34 73\n\
            \47 54 81 77 46\n\
            \\n\
            \12 83 25 82 72\n\
            \ 8  0 95  6 40\n\
            \17 64 27 23 91\n\
            \14 73 70 55 44\n\
            \69 76 92 78 56\n\
            \\n\
            \20 15 45 44 52\n\
            \94 26 61 38 64\n\
            \84 67 16 23 21\n\
            \73 71  5 10 36\n\
            \62 65  9 24 58\n\
            \\n\
            \59 75 60  0 97\n\
            \41 94 73 86 51\n\
            \ 8 89 22 45 18\n\
            \ 3 63 85 57 16\n\
            \42 44 10 23 93\n\
            \\n\
            \ 3 68 80 19 59\n\
            \41  6 92 58 28\n\
            \94 57 81  5 71\n\
            \90 54  9  8 14\n\
            \32 96 30 37 10\n\
            \\n\
            \15 16 14 10 52\n\
            \51 26 54 24 84\n\
            \45 90 28 36 96\n\
            \56 70 86 94 32\n\
            \67 81 13 29 27\n\
            \\n\
            \67 30 89 43  3\n\
            \86 10 38 90 77\n\
            \70 78 97 94 33\n\
            \29  8 85 69 56\n\
            \40 80 47 12 17"

data Board = Board [[Maybe Int]] deriving(Show)
--               Calls  Boards  Winning Board  Last Call  Boards Remaining
data Game = Game [Int] [Board]  (Maybe Board)  Int        Int                deriving(Show)

generateGame :: String -> Game
generateGame input = Game (generateCalls (head inlines)) boards Nothing (-1) (length boards)
  where boards = (fst (generateBoards (([], (tail( tail inlines))))))
        inlines = lines input

generateBoards :: ([Board], [String]) -> ([Board], [String])
generateBoards (boards, []) = (boards, [])
generateBoards (boards, bLines) = generateBoards (boards ++ [makeBoard (fst boardBreak)], if null (snd boardBreak) then [] else (tail ( snd boardBreak)))
  where boardBreak = span (not . null) bLines

generateCalls :: String -> [Int]
generateCalls c = map read (splitByCommas c)

splitByCommas :: String -> [String]
splitByCommas ""  = []
splitByCommas "," = []
splitByCommas s = (fst commaBreak) : if null (snd commaBreak) then [] else splitByCommas (tail $ snd commaBreak)
  where commaBreak = span (/= ',') s

call :: Game -> Game
call (Game calls boards _ _ _) = Game (tail calls) (filter (not . isBoardBingo) newBoards) (getBingo newBoards) (head calls) (length $ filter (not . isBoardBingo) newBoards)
  where newBoards = (markNumber (head calls) boards)

playOut :: Game -> Game
playOut (Game c b (Just wb) lastCall br) = Game c b (Just wb) lastCall br
playOut game = playOut (call game)

playOutUntilLastWinner :: Game -> Game
playOutUntilLastWinner (Game c [] wb lastCall _) = Game c [] wb lastCall 0
playOutUntilLastWinner game = playOutUntilLastWinner (call game)

answer :: Game -> Int 
answer (Game _ _ (Just wb) lastCall _) = lastCall * (sumUnmarked wb)
answer _ = -1

part1 :: Int 
part1 = answer $ playOut $ generateGame realData 

part2 :: Int 
part2 = answer $ playOutUntilLastWinner $ generateGame realData 

sumUnmarked :: Board -> Int 
sumUnmarked (Board rows) = sum (map intFromMaybe $ concat rows)

intFromMaybe :: (Maybe Int) -> Int 
intFromMaybe Nothing = 0
intFromMaybe (Just n) = n 

getBoardsFromGame :: Game -> [Board]
getBoardsFromGame (Game _ boards _ _ _) = boards

makeBoard :: [String] -> Board
makeBoard rows = Board (map (\row -> map (Just . read) row) (map words rows))

markNumber :: Int -> [Board] -> [Board]
markNumber n boards = map (markNumberOnBoard n) boards

markNumberOnBoard :: Int -> Board -> Board
markNumberOnBoard n (Board rows) = Board (map (markNumberInRow n) rows)

markNumberInRow :: Int -> [Maybe Int] -> [Maybe Int]
markNumberInRow n row = map (\s -> if s == (Just n) then Nothing else s) row

getBingo :: [Board] -> Maybe Board
getBingo [] = Nothing
getBingo (b:bs) = if (isBoardBingo b) then Just b else (getBingo bs)

isBoardBingo :: Board -> Bool 
isBoardBingo b = or [isHorizontalBingo b, isVerticalBingo b] -- diagonals don't count

isHorizontalBingo :: Board -> Bool 
isHorizontalBingo (Board rows) = any isBingoRow rows

isBingoRow :: [Maybe Int] -> Bool
isBingoRow row = all (== Nothing) row

isVerticalBingo :: Board -> Bool
isVerticalBingo (Board rows) = any id (map (\i -> isBingoColumn rows i) [0..(length (head rows) - 1)])

isBingoColumn :: [[Maybe Int]] -> Int -> Bool 
isBingoColumn rows index = isBingoRow $ map (!! index) rows

