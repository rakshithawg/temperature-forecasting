from tfrecords_handler.moving_window.tfrecord_writer import TFRecordWriter

if __name__ == '__main__':
    tfrecord_writer = TFRecordWriter(
        input_size = 120,
        output_size = 12,
        train_file_path = '/home/rakshitha/PycharmProjects/temperature-forecasting/datasets/text_data/moving_window/without_stl_decomposition/temperature_12i120.txt',
        validate_file_path = '/home/rakshitha/PycharmProjects/temperature-forecasting/datasets/text_data/moving_window/without_stl_decomposition/temperature_12i120v.txt',
        test_file_path = '/home/rakshitha/PycharmProjects/temperature-forecasting/datasets/text_data/moving_window/without_stl_decomposition/temperature_test_12i120.txt',
        binary_train_file_path = '/home/rakshitha/PycharmProjects/temperature-forecasting/datasets/binary_data/moving_window/without_stl_decomposition/temperature_12i120.tfrecords',
        binary_validation_file_path = '/home/rakshitha/PycharmProjects/temperature-forecasting/datasets/binary_data/moving_window/without_stl_decomposition/temperature_12i120v.tfrecords',
        binary_test_file_path = '/home/rakshitha/PycharmProjects/temperature-forecasting/datasets/binary_data/moving_window/without_stl_decomposition/temperature_test_12i120.tfrecords'
    )

    tfrecord_writer.read_text_data()
    tfrecord_writer.write_train_data_to_tfrecord_file()
    tfrecord_writer.write_validation_data_to_tfrecord_file()
    tfrecord_writer.write_test_data_to_tfrecord_file()