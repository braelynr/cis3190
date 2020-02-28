-- Braelyn Rotman
-- CIS*3190
-- Assignment 2
-- February 28, 2020

with ada.Text_IO; use Ada.Text_IO;
with ada.Directories; use ada.Directories;
with ada.strings.unbounded; use ada.strings.unbounded;
with ada.strings.unbounded.Text_IO; use ada.strings.unbounded.Text_IO;
with ada.characters.handling; use ada.characters.handling;
with ada.numerics.discrete_random;
procedure Wordscram is
    filename : unbounded_string;

    -- This function gets a filename from the user
    function getFilename return unbounded_string is
        name : unbounded_string;
        nameOK : boolean := false;
        -- name : the name of the file to process
        -- nameOK : to check if the file requested exists
    begin
        loop
            exit when nameOK;
            -- Ask for filename from user
            put("Enter the file name: ");
            get_line(name);
            -- Check if file exists
            nameOK := exists(to_string(name));
        end loop;
        return name;
    end getFilename;

    -- This function will check if a string is a word or a number
    function isWord (word : unbounded_string) return boolean is
        wordOK : boolean := true;
        i : Integer := 1;
        -- wordOK is true if the string is a word
        -- i is the index in the string
    begin
        -- Loops through each character of the string
        -- Loop will exit when the end of the string is reached or a digit is found
        loop
            exit when i = length(word) or wordOK = false;
            -- If a digit is found the string is a number
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
        -- randRange is the range of integers to select from
        -- gen is the random value generator
        -- num is a random value in the randRange
    begin
        -- Initialize the generator
        reset(gen);
        -- Create a random number
        num := random(gen);
        return Integer(num);
    end randomInt;

    -- This function will scramble a word leaving the first and last character intact
    function scrambleWord(toScramble : unbounded_string) return unbounded_string is
        output : unbounded_string := toScramble;
        l : constant Integer := length(toScramble);
        i, j : Integer;
    begin
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

    -- This procedure will process the text from the file
    procedure processText(filename : unbounded_string) is
        fp : file_type;
        line : unbounded_string;
        word : unbounded_string;
        i, j, l, wordCount : Integer;
        c, p : character;
        -- fp is the file pointer
        -- line is a line from the file
        -- word is a chunk of text taken from the line
        -- i & j are indices
        -- l is the length of the line
        -- wordCount is the number of words (including numbers) processed from the file
        -- c is the current character in the line and p is used to store the last char of the line
    begin
        wordCount := 0;
        open(fp,in_file,to_string(filename));
        loop
            exit when end_of_file(fp);
            -- Get each line from the file
            get_line(fp, line);
            l := length(line);
            -- i is the beginning index and j is the ending index for each word
            i := 1;
            j := 2;
            loop
                exit when i > l;
                c := 'a';

                -- This if statement and loop preserve punctuation and spaces between words
                if is_alphanumeric(element(line, i)) = false then
                    if (element(line, i) = ' ') then
                        put(" ");
                    else
                        put(element(line, i)); --element(line, i)));
                    end if;
                    i := i + 1;
                    j := i + 1;
                -- else its the start of the word
                else
                    -- Loop to find the substring indices of the next word
                    loop
                        exit when is_alphanumeric(c) = false or j > l;
                        c := element(line, j);
                        j := j + 1;
                    end loop;

                    --If it's the last word in the line with no trailing punctuation or space
                    if j >= l then
                        j := j + 1;
                    end if;
                    --get the word from the line
                    word := Unbounded_Slice(line, i, j - 2);

                    -- reset the indicies for the next word
                    i := j - 1;
                    j := i + 1;

                    --increase words processed count
                    wordCount := wordCount + 1;

                    p := ascii.nul;
                    -- If the last character of the line is not a letter it will need to be preserved
                    if(is_alphanumeric(element(word, length(word))) = false) then
                        -- save last char
                         p := element(word, length(word));
                         word := Unbounded_Slice(word, 1, length(word) - 1);
                    end if;

                    -- Scramble the word if needed and print
                    if (isWord(word) and length(word) > 3) then
                        -- if the string needs to be scrambled
                        put(scrambleWord(word));
                    else
                        -- If the string is a number or too short to scramble
                        put(word);
                    end if;

                    -- print the last line char if needed
                    if p /= ascii.nul then
                        put(p);
                    end if;
                end if;
            end loop;
            new_line;
        end loop;
        close(fp);

        -- Output the word count
        new_line;
        put_line("Number of Words (Includes Numbers) Processed From File: " & integer'image(wordCount));
    end processText;

begin

    -- Call the function to get the filename
    filename := getFilename;
    -- Send the filename for processing
    processText(filename);

end Wordscram;
