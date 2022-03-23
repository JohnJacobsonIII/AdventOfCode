import argparse
from functools import reduce


class ImageEnhancer(object):

    def __init__(self, image_enhancer, image):
        """
        Class initializer

        Parameters
        ----------
        image_enhancer : string
            String of light and dark pixels acting as an image enhancing algorithm
        image : list
            List of strings (2D matrix of pixels)

        Example:
        >>> object1 = load_data("test.txt")
        >>> assert len(object1.image_enhancer) == 512
        """
        assert len(image_enhancer) == 512
        self.image_enhancer = image_enhancer
        self.image = image

    def __str__(self):
        """
        String representation of this class

        >>> object1 = load_data("test.txt")
        >>> print(object1)
        Enhancer Length: 512
        #..#.
        #....
        ##..#
        ..#..
        ..###
        <BLANKLINE>
        """
        returning_str = "Enhancer Length".ljust(15) + ": " + str(len(self.image_enhancer)) + "\n"
        returning_str += self.return_image()

        return returning_str

    def enhance_image(self, iteration):
        """
        Using the image_enhancer, process the image.

        Parameters
        ----------
        iteration : int
            The iteration number of this enhancement

        Returns
        -------
        output_image : list[string]

        Example:
        >>> object1 = load_data("test.txt")
        >>> object1.image = object1.enhance_image(0)
        >>> object1.image
        ['.##.##.', '#..#.#.', '##.#..#', '####..#', '.#..##.', '..##..#', '...#.#.']
        >>> object1.image = object1.enhance_image(1)
        >>> object1.image
        ['.......#.', '.#..#.#..', '#.#...###', '#...##.#.', '#.....#.#', '.#.#####.', '..#.#####', '...##.##.', '....###..']
        """
        # output_image = ['.'*len(self.image[i]) for i in range(len(self.image))]
        output_image = []

        # Creating a shadow celled input image to avoid unnecessary 'if' conditions to process boundary pixels
        if iteration % 2 == 1 and self.image_enhancer[0] == '#':
            default_number = '1'
        else:
            default_number = '0'

        shadow_celled_input_image = [default_number * (len(self.image[0])+4)]*2
        for row in self.image:
            shadow_celled_input_image.append(default_number*2)
            row_replaced_hash_with_one = row.replace('#', '1')
            row_replaced_dot_with_zero = row_replaced_hash_with_one.replace('.', '0')
            shadow_celled_input_image[-1] += row_replaced_dot_with_zero
            shadow_celled_input_image[-1] += default_number*2
        shadow_celled_input_image.append(default_number * (len(self.image[0])+4))
        shadow_celled_input_image.append(default_number * (len(self.image[0])+4))

        assert len(shadow_celled_input_image) == len(shadow_celled_input_image[0]) == len(self.image)+4

        # Printing to check whether shadow celled image is extended and matches the input image.
        # for i in range(len(shadow_celled_input_image)):
        #     print(shadow_celled_input_image[i])
        # print(self.return_image())

        # Enhancing image
        # For each pixel that will be generated in output image,
        for i in range(1, len(shadow_celled_input_image)-1):
            output_image.append('')
            for j in range(1, len(shadow_celled_input_image[i])-1):

                # Generate a binary number from the 3x3 grid of 0's and 1's centered at the pixel
                binary_number = shadow_celled_input_image[i-1][j-1:j+2]
                binary_number += shadow_celled_input_image[i][j-1:j+2]
                binary_number += shadow_celled_input_image[i+1][j-1:j+2]

                # Convert binary number to decimal
                index_into_image_enhancer = int(binary_number, 2)

                # Index into the image_enhancer to get the output pixel value to be placed at the pixel being worked on
                output_image[-1] += self.image_enhancer[index_into_image_enhancer]

        assert len(output_image) == len(output_image[0]) == len(self.image)+2
        return output_image

    def count_light_pixels_after_n_enhances(self, n):
        """
        Enhance the image 'n' times and count the number of light pixels

        Parameters
        ----------
        n : int
            Number of times to enhance the image

        Returns
        -------
        int
            Number of light pixels in the final image

        Example:
        >>> object1 = load_data("test.txt")
        >>> object1.count_light_pixels_after_n_enhances(2)
        35
        """
        for i in range(n):
            self.image = self.enhance_image(i)

        # print(self.return_image())

        return reduce(lambda a, b: a + b.count('#'), self.image, 0)

    def return_image(self):
        """
        Returns the image in '.' (dark pixel) and '#' (light pixel) format.
        """
        returning_image = ""
        for i in range(len(self.image)):
            returning_image += self.image[i] + '\n'

        return returning_image


def load_data(file_name):
    input_image = []
    with open(file_name, 'r') as f:
        image_enhancer = f.readline().strip()
        f.readline()
        for line in f:
            input_image.append(line.strip())

    image_enhancing_object = ImageEnhancer(image_enhancer, input_image)

    return image_enhancing_object


def main():
    parser = argparse.ArgumentParser(description='AoC Day 20 Trench Map')
    parser.add_argument('-f', '--file',
                        help='Input file with an image enhancing algorithm string and 2D image.',
                        default='input.txt')
    parser.add_argument('-c', '--code',
                        help='Select 1: Count \'#\' (light pixels) after 2 enhancements or 2: Count \'#\' (light '
                             'pixels) after 50 enhancements.',
                        type=int,
                        default=1)
    arguments = parser.parse_args()

    image_enhancing_object = load_data(arguments.file)
    # print(image_enhancing_object)

    if arguments.code == 1:
        print(image_enhancing_object.count_light_pixels_after_n_enhances(2))
    elif arguments.code == 2:
        print(image_enhancing_object.count_light_pixels_after_n_enhances(50))
    else:
        print("Selected code not valid")


if __name__ == "__main__":
    main()
