##############################################################################
# (c) Crown copyright 2020 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
import filecmp
from pathlib import Path

from metadata_extractor import MetadataExtractor
from metadata_iodef_generator import MetadataIodefGenerator
from metadata_validator import validate_metadata

TEST_DIR = Path(__file__).parent
IMMUTABLE_DATA_PATH = TEST_DIR / Path('input/LFRic_meta_data.JSON')
ROSE_APP_PATH = TEST_DIR / Path('input/rose-suite/rose-app.conf')
TEST_IODEF_PATH1 = TEST_DIR / Path('kgos/test_iodef.xml')
OUTPUT_PATH1 = TEST_DIR / Path('output/iodef.xml')

TEMPLATE_PATH = str(TEST_DIR / 'templates/minimal_iodef.xml')
TEST_IODEF_PATH2 = TEST_DIR / Path('kgos/test_iodef2.xml')
OUTPUT_PATH2 = TEST_DIR / Path('output/iodef_from_template.xml')


def test_run():
    extractor = MetadataExtractor(ROSE_APP_PATH, IMMUTABLE_DATA_PATH)
    metadata = extractor.extract_metadata()
    validate_metadata(metadata)
    generator = MetadataIodefGenerator(metadata)
    generator.generate(OUTPUT_PATH1)

    assert filecmp.cmp(TEST_IODEF_PATH1, OUTPUT_PATH1)


def test_with_template():
    extractor = MetadataExtractor(ROSE_APP_PATH, IMMUTABLE_DATA_PATH)
    metadata = extractor.extract_metadata()
    validate_metadata(metadata)
    generator = MetadataIodefGenerator(metadata)
    generator.generate(OUTPUT_PATH2, TEMPLATE_PATH)

    assert filecmp.cmp(TEST_IODEF_PATH2, OUTPUT_PATH2)
