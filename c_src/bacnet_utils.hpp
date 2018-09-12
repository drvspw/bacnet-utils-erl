#ifndef XCLIENT_BACNET_UTILS_HPP
#define XCLIENT_BACNET_UTILS_HPP
#pragma once

#include <bacapp.h>

#include <cstddef>

uint8_t build_write_property_request(uint8_t* buffer,
                                     uint8_t* tmp_buffer,
                                     BACNET_APPLICATION_DATA_VALUE * object_value);

uint8_t build_read_property_request(uint8_t* buffer, uint8_t* tmp_buffer, BACNET_OBJECT_TYPE object_type,
				    uint32_t object_instance, BACNET_PROPERTY_ID object_property, uint32_t array_index);

bool get_apdu_from_message(uint8_t* message, uint8_t** apdu_out, uint16_t* apdu_len);

BACNET_PDU_TYPE get_pdu_type(uint8_t* apdu);

bool get_value_from_complex_ack(uint8_t* apdu, uint16_t apdu_len, BACNET_APPLICATION_DATA_VALUE& value);

uint8_t* prepare_bacnet_octet_string_payload(BACNET_APPLICATION_DATA_VALUE& out, std::size_t size);

void build_bacnet_payload(BACNET_APPLICATION_DATA_VALUE& out, uint64_t test_id, uint64_t next_tag_);

#endif
