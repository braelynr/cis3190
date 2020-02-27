with ada.Text_IO; use Ada.Text_IO;
with ada.Directories; use ada.Directories;
with ada.strings.unbounded; use ada.strings.unbounded;
with ada.strings.unbounded.Text_IO; use ada.strings.unbounded.Text_IO;
with ada.characters.handling; use ada.characters.handling;
with ada.numerics.discrete_random;
procedure Wordscram is
    filename : unbounded_string;

    function getFilename return unbounded_string is
        name : unbounded_string;
        nameOK : boolean := false;
    begin
        loop
            exit when nameOK;
            put("Enter the file name: ");
            get_line(name);
            nameOK := exists(to_string(name));
        end loop;
        return name;
    end getFilename;

    function isWord (word : unbounded_string) return boolean is
        wordOK : boolean := true;
        i : Integer := 1;
    begin
        loop
            exit when i = length(word) or wordOK = false;
            if is_digit(element(word, i)) then
                wordOK := false;
            end if;
            i := i + 1;
        end loop;
        return wordOK;
    end isWord;

    -- Function to create a random integer between 2 numbers A and B
    function randomInt(a, b : Integer) return Integer is
        type randRange is new Integer range a..b;
        package Rand_Int is new ada.numerics.discrete_random(randRange);
        use Rand_Int;
        gen : Generator;
        num : randRange;
    begin
        reset(gen);
        num := random(gen);
        return Integer(num);
    end randomInt;

    function scrambleWord(toScramble : unbounded_string) return unbounded_string is
        output : unbounded_string := toScramble;
        l : constant Integer := length(toScramble);
        i, j : Integer;
    begin
        --Replace_Slice(output,2,length(output)-1,"***");
        -- This loop will temporarily replace the middle letters of the output string
        i := 2;
        loop
            exit when i = l;
            replace_element(output, i, '*');
            i := i + 1;
        end loop;

        j := 2;
        -- This loop is to put the letters back in a random order
        loop
            exit when j = l;
            -- Loop until an unused random number is found
            loop
                exit when element(output, i) = '*';
                i := randomInt(2, l - 1);
            end loop;
            -- Take the next element from the original word and place it in the random spot
            replace_element(output, i, element(toScramble, j));

            j := j + 1;
        end loop;

        return output;
    end scrambleWord;

    --WHAT ABOUT THE NUMBER OF WORDS PROCESSED

    procedure processText(filename : unbounded_string) is
        fp : file_type;
        line : unbounded_string;
        word : unbounded_string;
        i, j, l, wordCount : Integer;
        c, p : character;
    begin
        wordCount := 0;
        open(fp,in_file,to_string(filename));
        loop
            exit when end_of_file(fp);
            get_line(fp, line);
            l := length(line);
            i := 1;
            j := 2;
            loop
                exit when i >= l;
                c := 'a';

                -- This if statement and loop preserve multiple spaces between words
                if element(line, i) = ' ' then
                    loop
                        exit when element(line, i) /= ' ';
                        put(" ");
                        i := i + 1;
                    end loop;
                    j := i + 1;
                end if;

                -- Loop to find the substring indices of the next word
                loop
                    exit when c = ' ' or j >= length(line);
                    c := element(line, j);
                    j := j + 1;
                end loop;
                if j >= length(line) then
                    j := j + 2;
                end if;
                word := Unbounded_Slice(line, i, j - 2);

                i := j;
                j := i + 1;

                wordCount := wordCount + 1;

                p := ascii.nul;
                -- Check if punctuation needs to be picked off first
                if(is_special(element(word, length(word)))) then
                    -- save punctuation char
                     p := element(word, length(word));
                     word := Unbounded_Slice(word, 1, length(word) - 1);
                end if;

                -- Scramble the word if needed and print
                if (isWord(word) and length(word) > 3) then
                    -- if the word needs to be scrambled
                    put(scrambleWord(word));
                else
                    -- If the word is a number
                    put(word);
                end if;

                -- print the punctuation if needed
                if p /= ascii.nul then
                    put(p);
                end if;
                put(" ");

            end loop;
            new_line;
        end loop;
        close(fp);

        -- Output the word count
        new_line;
        put_line("Number of Words (Includes Numbers) Processed From File: " & integer'image(wordCount));
    end processText;

begin

    filename := getFilename;
    processText(filename);

end Wordscram;
