#include "bacnet_utils.hpp"

#include <bacdef.h>
#include <config.h>
#include <bactext.h>
#include <bacerror.h>
#include <rp.h>
#include <wp.h>
#include <npdu.h>
#include <bip.h>
#include <bacint.h>
#include <bacenum.h>
#include <apdu.h>
#include <bits.h>
#include <bacapp.h>

#include <assert.h>

#include <cstring>

namespace {
  uint8_t build_bvll(uint8_t* buffer, uint8_t * pdu, unsigned pdu_len);

  uint8_t decode_bvll(uint8_t* bvll_in, uint8_t** npdu);

  uint16_t decode_npdu(uint8_t* npdu_in, uint16_t npdu_len, uint8_t** apdu);

  BACNET_OBJECT_TYPE object_type_k = OBJECT_OCTETSTRING_VALUE;
  uint32_t object_instance_k = 0;
  BACNET_PROPERTY_ID object_property_k = PROP_PRESENT_VALUE;
  uint32_t array_index_k = -1;  /* No array, just a scalar value */
  uint8_t priority_k = 1;   /* Make the write high priority */
}   // anonymous namespace

uint8_t build_write_property_request(uint8_t* buffer,
                                     uint8_t* tmp_buffer,
                                     BACNET_APPLICATION_DATA_VALUE * object_value)
{
  uint8_t invoke_id = 0;
  int second_len = 0;
  int pdu_len = 0;
  BACNET_WRITE_PROPERTY_DATA data;
  BACNET_NPDU_DATA npdu_data;

  uint8_t application_data[MAX_APDU] = { 0 };
  int apdu_len = 0, first_len = 0;

  /* encode the data */
  while (object_value) {
    first_len = bacapp_encode_data(&application_data[apdu_len], object_value);
    if ((first_len + apdu_len) < MAX_APDU) {
      apdu_len += first_len;
    } else {
      return 0;
    }
    object_value = object_value->next;
  }

  /* encode the NPDU portion of the packet */
  npdu_encode_npdu_data(&npdu_data, true, MESSAGE_PRIORITY_NORMAL);
  pdu_len =
    npdu_encode_pdu(&tmp_buffer[0], nullptr, nullptr,
		    &npdu_data);
  /* encode the APDU portion of the packet */
  data.object_type = object_type_k;
  data.object_instance = object_instance_k;
  data.object_property = object_property_k;
  data.array_index = array_index_k;
  data.application_data_len = apdu_len;
  memcpy(&data.application_data[0], &application_data[0],
	 apdu_len);
  data.priority = priority_k;
  second_len =
    wp_encode_apdu(&tmp_buffer[pdu_len], invoke_id,
		   &data);
  pdu_len += second_len;

  return build_bvll(buffer,
		    &tmp_buffer[0], pdu_len);
}

uint8_t build_read_property_request(uint8_t* buffer, uint8_t* tmp_buffer)
{
  uint8_t invoke_id = 0;
  int len = 0;
  int pdu_len = 0;
  BACNET_READ_PROPERTY_DATA data;
  BACNET_NPDU_DATA npdu_data;

  /* encode the NPDU portion of the packet */
  npdu_encode_npdu_data(&npdu_data, true, MESSAGE_PRIORITY_NORMAL);
  pdu_len =
    npdu_encode_pdu(&tmp_buffer[0], nullptr, nullptr,
		    &npdu_data);
  /* encode the APDU portion of the packet */
  data.object_type = object_type_k;
  data.object_instance = object_instance_k;
  data.object_property = object_property_k;
  data.array_index = array_index_k;
  len =
    rp_encode_apdu(&tmp_buffer[pdu_len], invoke_id,
		   &data);
  pdu_len += len;

  return build_bvll(buffer, &tmp_buffer[0], pdu_len);
}

bool get_apdu_from_message(uint8_t* message, uint8_t** apdu_out, uint16_t* apdu_len)
{
  uint8_t* npdu;
  uint8_t npdu_len = decode_bvll(message, &npdu);
  if (npdu_len == 0) {
    return false;
  }

  *apdu_len = decode_npdu(npdu, npdu_len, apdu_out);
  if (apdu_len == 0) {
    return false;
  }

  return true;
}

BACNET_PDU_TYPE get_pdu_type(uint8_t* apdu)
{
  return static_cast<BACNET_PDU_TYPE>(apdu[0] & 0xF0);
}

bool get_value_from_complex_ack(uint8_t* apdu, uint16_t apdu_len, BACNET_APPLICATION_DATA_VALUE& value)
{
  uint16_t len = 2;
  uint8_t service_choice = apdu[len++];
  uint8_t* service_request = &apdu[len];
  uint16_t service_request_len = apdu_len - len;
  if (service_choice != SERVICE_CONFIRMED_READ_PROPERTY) {
    return false;
  }

  BACNET_READ_PROPERTY_DATA data;
  len = rp_ack_decode_service_request(service_request, service_request_len, &data);
  if (len > 0) {
    bacapp_decode_application_data(data.application_data,
				   static_cast<uint8_t>(data.application_data_len),
				   &value);

    // ZVB 2017-06-27: We're assuming we only asked for a single value
    assert(value.next == NULL);
  } else {
    return false;
  }

  return true;
}

uint8_t* prepare_bacnet_octet_string_payload(BACNET_APPLICATION_DATA_VALUE& out, std::size_t size)
{
  out.context_specific = false;
  out.tag = BACNET_APPLICATION_TAG_OCTET_STRING;
  out.next = NULL;
  out.type.Octet_String.length = size;

  return out.type.Octet_String.value;
}

  void build_bacnet_payload(BACNET_APPLICATION_DATA_VALUE& out, uint64_t test_id, uint64_t next_tag_)
  {
    uint8_t* payload = prepare_bacnet_octet_string_payload(out, sizeof(test_id) + sizeof(next_tag_));
    std::memcpy(payload, &test_id, sizeof(test_id));
    std::memcpy(payload + sizeof(test_id), &next_tag_, sizeof(next_tag_));
  }

namespace {
  uint8_t build_bvll(uint8_t* buffer, uint8_t * pdu, unsigned pdu_len)
  {
    uint16_t mtu_len = 0;
    uint16_t BVLC_length = 0;

    buffer[0] = BVLL_TYPE_BACNET_IP;
    /* ZVB 06-15-2017: We only handle unicast */
    buffer[1] = BVLC_ORIGINAL_UNICAST_NPDU;

    BVLC_length = (uint16_t) pdu_len + 4 /*inclusive */ ;
    mtu_len = 2;
    mtu_len += (uint16_t) encode_unsigned16(&buffer[mtu_len], BVLC_length);
    memcpy(&buffer[mtu_len], pdu, pdu_len);
    mtu_len += (uint16_t) pdu_len;

    return mtu_len;
  }

  uint8_t decode_bvll(uint8_t* bvll_in, uint8_t** npdu)
  {
    BACNET_BVLC_FUNCTION BVLC_Function_Code = (BACNET_BVLC_FUNCTION)bvll_in[1];
    if (BVLC_Function_Code == BVLC_ORIGINAL_UNICAST_NPDU) {
      /* decode the length of the PDU - length is inclusive of BVLC */
      uint16_t npdu_len = 0;
      (void) decode_unsigned16(&bvll_in[2], &npdu_len);

      /* subtract off the BVLC header */
      npdu_len -= 4;
      *npdu = bvll_in + 4;

      return npdu_len;
    } else {
      *npdu = nullptr;
      return 0;
    }
  }

  uint16_t decode_npdu(uint8_t* npdu_in, uint16_t npdu_len, uint8_t** apdu)
  {
    BACNET_NPDU_DATA npdu_data;

    if (npdu_in[0] != BACNET_PROTOCOL_VERSION) {
      *apdu = nullptr;
      return 0;
    }

    int apdu_offset = npdu_decode(&npdu_in[0], nullptr, nullptr, &npdu_data);

    if (npdu_data.network_layer_message) {
      *apdu = nullptr;
      return 0;
    }

    if ((apdu_offset <= 0) || (apdu_offset > npdu_len)) {
      *apdu = nullptr;
      return 0;
    }

    *apdu = &npdu_in[apdu_offset];
        
    return npdu_len - apdu_offset;
  }
}   // anonymous namespace
