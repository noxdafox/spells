"""Turns a Pytnon INI file into a dictionary.

Example

my_config.ini

[section]

value = string
other_value = 1

[other_section]

value = 0

parse_configuration('my_config.ini')

{
    'section':
        {
            'value': 'string',
            'other_value': '1'
        },
    'other_section':
        {
            'value': '0'
        }
}

"""

from collections import defaultdict

try:
    from configparser import ConfigParser
except ImportError:
    from ConfigParser import ConfigParser


def parse_configuration(path):
    """Turns a Python INI file into a nested dict[section][option]."""
    configuration = defaultdict(dict)
    parser = ConfigParser()

    parser.read(path)

    for section in parser.sections():
        for option in parser.options(section):
            configuration[section][option] = parser.get(section, option)

    return configuration
