##############################################################################
# (c) Crown copyright 2020 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
import pytest
from pathlib import Path

from metadata_extractor import MetadataExtractor

TEST_DIR = Path(__file__).parent
IMMUTABLE_DATA_PATH = TEST_DIR / Path('input/LFRic_meta_data_test.JSON')
IMMUTABLE_DATA_NO_CHECKSUM_PATH = TEST_DIR / Path(
    'input/LFRic_meta_data_no_checksum.JSON')
IMMUTABLE_DATA_BAD_CHECKSUM_PATH = TEST_DIR / Path(
    'input/LFRic_meta_data_bad_checksum.JSON')
IMMUTABLE_DATA_BAD_FUNCTION_SPACE_PATH = TEST_DIR / Path(
    'input/LFRic_meta_data_bad_function_space.JSON')
ROSE_SUITE_PATH = TEST_DIR / Path('input/rose-suite/rose-app.conf')
BAD_ADDITIONAL_INPUT_ROSE_SUITE_PATH = TEST_DIR / Path(
    'input/rose-suite-bad-additional-input/rose-app.conf')
BAD_ROSE_SUITE_PATH = TEST_DIR / Path('input/rose-app.conf')
BAD_VERT_DIM_ROSE_SUITE_PATH = TEST_DIR / Path(
        'input/rose-suite/rose-app-invalid-vert-dim.conf')


def test_extractor():
    """Test that sample metadata is correctly parsed"""
    immutable_metadata = {
        "meta_data": {
            "sections": {
                "section_name": {
                    "groups": {
                        "field_group_1": {
                            "fields": {
                                "section_name__field_1": {
                                    "_unique_id": "section_name__field_1",
                                    "units": "units_1"},
                                "section_name__field_2": {
                                    "_unique_id": "section_name__field_2",
                                    "units": "units_2"
                                }
                            }
                        },
                        "field_group_2": {
                            "fields": {
                                "section_name__field_3": {
                                    "_unique_id": "section_name__field_3",
                                    "units": "units_3"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    extractor = MetadataExtractor(ROSE_SUITE_PATH, IMMUTABLE_DATA_PATH)
    assert extractor._immutable_metadata == immutable_metadata


def test_extractor_no_checksum():
    """Test that exception is raised if there is no checksum in the immutable
    metadata file"""
    with pytest.raises(KeyError) as excinfo:
        MetadataExtractor(ROSE_SUITE_PATH, IMMUTABLE_DATA_NO_CHECKSUM_PATH)
    assert "Can't find checksum" in str(excinfo.value)


def test_extractor_incorrect_checksum():
    """Test that exception is raised if the checksum of the immutable metadata
    does not match the checksum in the immutable metadata file"""
    with pytest.raises(RuntimeError) as excinfo:
        MetadataExtractor(ROSE_SUITE_PATH,
                          IMMUTABLE_DATA_BAD_CHECKSUM_PATH)
    assert "8fec930ffa6141918a19ed2aa6e6a75d" in str(excinfo.value)


def test_extractor_no_rose_app_conf():
    """Test that exception is raised if there is no rose-app.conf at the path
    given to the MetadataExtractor"""
    with pytest.raises(IOError) as excinfo:
        MetadataExtractor(BAD_ROSE_SUITE_PATH, IMMUTABLE_DATA_PATH)
    assert "Could not find rose-app.conf" in str(excinfo.value)


def test_extractor_bad_additional_input():
    """Test that exception is raised when an unrecognised additional input
    for a field is included in the rose-app.conf"""
    with pytest.raises(ValueError) as excinfo:
        extractor = MetadataExtractor(BAD_ADDITIONAL_INPUT_ROSE_SUITE_PATH,
                                      IMMUTABLE_DATA_PATH)
        extractor.extract_metadata()
    assert "Unrecognised additional field input 'nonsense' found for field " \
           "'boundary_layer__air_temperature_over_tiles'" in str(excinfo.value)


def test_extractor_invalid_function_space():
    """Test that exception is raised when a field has a function space that
    is not supported by the reconfigurator"""
    with pytest.raises(ValueError) as excinfo:
        extractor = MetadataExtractor(
                ROSE_SUITE_PATH, IMMUTABLE_DATA_BAD_FUNCTION_SPACE_PATH)
        extractor.extract_metadata()
    assert "Invalid function space W42" in str(excinfo.value)


def test_extractor_invalid_vert_dim(caplog):
    """Test that exception is raised when vertical dimension is invalid and
    that all missing attributes are logged as missing"""
    with pytest.raises(ValueError):
        extractor = MetadataExtractor(BAD_VERT_DIM_ROSE_SUITE_PATH,
                                      IMMUTABLE_DATA_PATH)
        extractor.extract_metadata()
        assert "'model_vert_axis_1' missing 'name' attribute" \
               in caplog.text
        assert "'model_vert_axis_1' missing 'number_of_layers' attribute" \
               in caplog.text
        assert "'model_vert_axis_1' missing 'positive_direction' attribute" \
               in caplog.text
        assert "'model_vert_axis_1' missing 'primary_axis' attribute" \
               in caplog.text
        assert "'model_vert_axis_1' missing 'units' attribute" \
               in caplog.text
        assert "'model_vert_axis_1' requires either a domain top and " \
               "extrusion method or a level definition" in caplog.text
        assert "'model_vert_axis_2' requires either a domain top and " \
               "extrusion method or a level definition" in caplog.text
        assert "'model_vert_axis_3' requires either a domain top and " \
               "extrusion method or a level definition" in caplog.text
        assert "'model_vert_axis_4' requires either a domain top and " \
               "extrusion method or a level definition" in caplog.text
        assert "'model_vert_axis_5' requires either a domain top and " \
               "extrusion method or a level definition" in caplog.text
