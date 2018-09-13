#include <cstring>

#include "erl_nif.h"
#include "bacnet_utils.hpp"

#define OK "ok"
#define BUFFER_SIZE 1500

ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* mesg)
{
    return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

static ERL_NIF_TERM
build_write_msv_req_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  uint32_t object_instance = 0;
  uint32_t value = 0;
  uint32_t array_index = -1;
  
  if(
     argc != 2 ||
     !enif_get_uint(env, argv[0], &object_instance) ||
     !enif_get_uint(env, argv[1], &value)
     ) {
    return enif_make_badarg(env);
  }

  uint8_t write_buffer[BUFFER_SIZE];
  uint8_t tmp_buffer[BUFFER_SIZE];

    BACNET_OBJECT_TYPE object_type = OBJECT_MULTI_STATE_VALUE;
  BACNET_PROPERTY_ID object_property = PROP_PRESENT_VALUE;
  
  BACNET_APPLICATION_DATA_VALUE bacnet_payload[1];
  build_bacnet_payload(bacnet_payload[0], value);
  uint8_t wp_len = build_write_property_request(write_buffer, tmp_buffer, object_type, object_instance, object_property, array_index, bacnet_payload);

  if( !wp_len ) {
    return mk_error(env, "write_msv_request_error");
  }

  ErlNifBinary wp;
  if (!enif_alloc_binary(wp_len, &wp)) {
    return mk_error(env, "alloc_failed");
  }

  // Copy data to the ErlNifBinary
  std::memcpy(wp.data, write_buffer, wp_len);

  // return tuple
  ERL_NIF_TERM ok = mk_atom(env, OK);
  ERL_NIF_TERM wpBin = enif_make_binary(env, &wp);

  return enif_make_tuple2(env, ok, wpBin);
}

static ERL_NIF_TERM
build_write_octetstring_req_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifUInt64 id, tag;
  uint32_t object_instance = 0;
  uint32_t array_index = -1;
  
  if(
     argc != 3 ||
     !enif_get_uint(env, argv[0], &object_instance) ||
     !enif_get_uint64(env, argv[1], &id) ||
     !enif_get_uint64(env, argv[2], &tag)
     ) {
    return enif_make_badarg(env);
  }

  uint8_t write_buffer[BUFFER_SIZE];
  uint8_t tmp_buffer[BUFFER_SIZE];

  BACNET_OBJECT_TYPE object_type = OBJECT_OCTETSTRING_VALUE;
  BACNET_PROPERTY_ID object_property = PROP_PRESENT_VALUE;
  
  BACNET_APPLICATION_DATA_VALUE bacnet_payload[1];
  build_bacnet_payload(bacnet_payload[0], id, tag);
  
  uint8_t wp_len = build_write_property_request(write_buffer, tmp_buffer, object_type, object_instance, object_property, array_index, bacnet_payload);

  if( !wp_len ) {
    return mk_error(env, "write_octet_request_error");
  }

  ErlNifBinary wp;
  if (!enif_alloc_binary(wp_len, &wp)) {
    return mk_error(env, "alloc_failed");
  }

  // Copy data to the ErlNifBinary
  std::memcpy(wp.data, write_buffer, wp_len);

  // return tuple
  ERL_NIF_TERM ok = mk_atom(env, OK);
  ERL_NIF_TERM wpBin = enif_make_binary(env, &wp);

  return enif_make_tuple2(env, ok, wpBin);
}

static ERL_NIF_TERM
build_read_octetstring_pv_req_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  uint32_t object_instance = 0;
  uint32_t array_index = -1;
  
  if(argc != 1 ||
     !enif_get_uint(env, argv[0], &object_instance)
     ) {
    return enif_make_badarg(env);
  }

  uint8_t buffer[BUFFER_SIZE];
  uint8_t tmp_buffer[BUFFER_SIZE];

  BACNET_OBJECT_TYPE object_type = OBJECT_OCTETSTRING_VALUE;
  BACNET_PROPERTY_ID object_property = PROP_PRESENT_VALUE;

  uint8_t rp_len = build_read_property_request(buffer, tmp_buffer, object_type, object_instance, object_property, array_index);
  if( !rp_len ) {
    return mk_error(env, "read_octet_request_error");
  }

  ErlNifBinary rp;
  if (!enif_alloc_binary(rp_len, &rp)) {
    return mk_error(env, "alloc_failed");
  }

  // Copy data to the ErlNifBinary
  std::memcpy(rp.data, buffer, rp_len);

  // return tuple
  ERL_NIF_TERM ok = mk_atom(env, OK);
  ERL_NIF_TERM rpBin = enif_make_binary(env, &rp);

  return enif_make_tuple2(env, ok, rpBin);
}

static ERL_NIF_TERM
build_read_analog_value_pv_req_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  uint32_t object_instance = 0;
  uint32_t array_index = -1;
  
  if(argc != 1 ||
     !enif_get_uint(env, argv[0], &object_instance)
     ) {
    return enif_make_badarg(env);
  }

  uint8_t buffer[BUFFER_SIZE];
  uint8_t tmp_buffer[BUFFER_SIZE];

  BACNET_OBJECT_TYPE object_type = OBJECT_ANALOG_VALUE;
  BACNET_PROPERTY_ID object_property = PROP_PRESENT_VALUE;

  uint8_t rp_len = build_read_property_request(buffer, tmp_buffer, object_type, object_instance, object_property, array_index);
  if( !rp_len ) {
    return mk_error(env, "read_analog_value_request_error");
  }

  ErlNifBinary rp;
  if (!enif_alloc_binary(rp_len, &rp)) {
    return mk_error(env, "alloc_failed");
  }

  // Copy data to the ErlNifBinary
  std::memcpy(rp.data, buffer, rp_len);

  // return tuple
  ERL_NIF_TERM ok = mk_atom(env, OK);
  ERL_NIF_TERM rpBin = enif_make_binary(env, &rp);

  return enif_make_tuple2(env, ok, rpBin);
}

static ERL_NIF_TERM
build_read_analog_input_oos_req_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  uint32_t object_instance = 0;
  uint32_t array_index = -1;
  
  if(argc != 1 ||
     !enif_get_uint(env, argv[0], &object_instance)
     ) {
    return enif_make_badarg(env);
  }

  uint8_t buffer[BUFFER_SIZE];
  uint8_t tmp_buffer[BUFFER_SIZE];

  BACNET_OBJECT_TYPE object_type = OBJECT_ANALOG_INPUT;
  BACNET_PROPERTY_ID object_property = PROP_OUT_OF_SERVICE;

  uint8_t rp_len = build_read_property_request(buffer, tmp_buffer, object_type, object_instance, object_property, array_index);
  if( !rp_len ) {
    return mk_error(env, "read_analog_input_request_error");
  }

  ErlNifBinary rp;
  if (!enif_alloc_binary(rp_len, &rp)) {
    return mk_error(env, "alloc_failed");
  }

  // Copy data to the ErlNifBinary
  std::memcpy(rp.data, buffer, rp_len);

  // return tuple
  ERL_NIF_TERM ok = mk_atom(env, OK);
  ERL_NIF_TERM rpBin = enif_make_binary(env, &rp);

  return enif_make_tuple2(env, ok, rpBin);
}

static ERL_NIF_TERM
build_read_analog_input_pv_req_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  uint32_t object_instance = 0;
  uint32_t array_index = -1;
  
  if(argc != 1 ||
     !enif_get_uint(env, argv[0], &object_instance)
     ) {
    return enif_make_badarg(env);
  }

  uint8_t buffer[BUFFER_SIZE];
  uint8_t tmp_buffer[BUFFER_SIZE];

  BACNET_OBJECT_TYPE object_type = OBJECT_ANALOG_INPUT;
  BACNET_PROPERTY_ID object_property = PROP_PRESENT_VALUE;

  uint8_t rp_len = build_read_property_request(buffer, tmp_buffer, object_type, object_instance, object_property, array_index);
  if( !rp_len ) {
    return mk_error(env, "read_analog_input_request_error");
  }

  ErlNifBinary rp;
  if (!enif_alloc_binary(rp_len, &rp)) {
    return mk_error(env, "alloc_failed");
  }

  // Copy data to the ErlNifBinary
  std::memcpy(rp.data, buffer, rp_len);

  // return tuple
  ERL_NIF_TERM ok = mk_atom(env, OK);
  ERL_NIF_TERM rpBin = enif_make_binary(env, &rp);

  return enif_make_tuple2(env, ok, rpBin);
}

static ERL_NIF_TERM
build_read_analog_output_pv_req_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  uint32_t object_instance = 0;
  uint32_t array_index = -1;
  
  if(argc != 1 ||
     !enif_get_uint(env, argv[0], &object_instance)
     ) {
    return enif_make_badarg(env);
  }
  
  uint8_t buffer[BUFFER_SIZE];
  uint8_t tmp_buffer[BUFFER_SIZE];

  BACNET_OBJECT_TYPE object_type = OBJECT_ANALOG_OUTPUT;
  BACNET_PROPERTY_ID object_property = PROP_PRESENT_VALUE;

  uint8_t rp_len = build_read_property_request(buffer, tmp_buffer, object_type, object_instance, object_property, array_index);
  if( !rp_len ) {
    return mk_error(env, "read_analog_output_request_error");
  }

  ErlNifBinary rp;
  if (!enif_alloc_binary(rp_len, &rp)) {
    return mk_error(env, "alloc_failed");
  }

  // Copy data to the ErlNifBinary
  std::memcpy(rp.data, buffer, rp_len);

  // return tuple
  ERL_NIF_TERM ok = mk_atom(env, OK);
  ERL_NIF_TERM rpBin = enif_make_binary(env, &rp);

  return enif_make_tuple2(env, ok, rpBin);
}

static ERL_NIF_TERM
build_read_msv_pv_req_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  uint32_t object_instance = 0;
  uint32_t array_index = -1;
  
  if(argc != 1 ||
     !enif_get_uint(env, argv[0], &object_instance)
     ) {
    return enif_make_badarg(env);
  }

  uint8_t buffer[BUFFER_SIZE];
  uint8_t tmp_buffer[BUFFER_SIZE];

  BACNET_OBJECT_TYPE object_type = OBJECT_MULTI_STATE_VALUE;
  BACNET_PROPERTY_ID object_property = PROP_PRESENT_VALUE;

  uint8_t rp_len = build_read_property_request(buffer, tmp_buffer, object_type, object_instance, object_property, array_index);
  if( !rp_len ) {
    return mk_error(env, "read_msv_request_error");
  }

  ErlNifBinary rp;
  if (!enif_alloc_binary(rp_len, &rp)) {
    return mk_error(env, "alloc_failed");
  }

  // Copy data to the ErlNifBinary
  std::memcpy(rp.data, buffer, rp_len);

  // return tuple
  ERL_NIF_TERM ok = mk_atom(env, OK);
  ERL_NIF_TERM rpBin = enif_make_binary(env, &rp);

  return enif_make_tuple2(env, ok, rpBin);
}

static ERL_NIF_TERM
get_apdu_from_message_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary msg;
  
  if(
     argc != 1 ||
     !enif_inspect_binary(env, argv[0], &msg)
     ) {
    return enif_make_badarg(env);
  }

  uint8_t* apdu;
  uint16_t apdu_len;

  uint8_t * buffer = reinterpret_cast<uint8_t *>(msg.data);
  if (!get_apdu_from_message(buffer, &apdu, &apdu_len)) {
    return mk_error(env, "apdu_parse_error");
  }

  ErlNifBinary apduBin;
  if (!enif_alloc_binary(apdu_len, &apduBin)) {
    return mk_error(env, "alloc_failed");
  }

  // Copy data to the ErlNifBinary
  std::memcpy(apduBin.data, apdu, apdu_len);

  // return tuple
  ERL_NIF_TERM ok = mk_atom(env, OK);
  ERL_NIF_TERM apduBinTerm = enif_make_binary(env, &apduBin);

  return enif_make_tuple2(env, ok, apduBinTerm);
}

static ERL_NIF_TERM
get_pdu_type_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary apduBin;
  
  if(
     argc != 1 ||
     !enif_inspect_binary(env, argv[0], &apduBin)
     ) {
    return enif_make_badarg(env);
  }

  uint8_t* apdu =  reinterpret_cast<uint8_t *>(apduBin.data);

  switch (get_pdu_type(apdu)) {
  case PDU_TYPE_SIMPLE_ACK:
    return mk_atom(env, "pdu_type_simple_ack");
    
  case PDU_TYPE_COMPLEX_ACK:
    return mk_atom(env, "pdu_type_complex_ack");

  default:
    return mk_error(env, "pdu_type_not_recognized");
  }
}

static ERL_NIF_TERM
get_value_from_complex_ack_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary apduBin;
  
  if(
     argc != 1 ||
     !enif_inspect_binary(env, argv[0], &apduBin)
     ) {
    return enif_make_badarg(env);
  }

  uint8_t* apdu =  reinterpret_cast<uint8_t *>(apduBin.data);
  uint16_t apdu_len = apduBin.size;
  

  BACNET_APPLICATION_DATA_VALUE value;
  bool value_success = get_value_from_complex_ack(apdu, apdu_len, value);
  if (!value_success) {
    return mk_error(env, "apdu_value_error");
  }

  if (value.tag != BACNET_APPLICATION_TAG_OCTET_STRING) {
    return mk_error(env, "invalid_octet_string");
  }
  
  // get octet string in the bacnet packet
  uint8_t* received_value = value.type.Octet_String.value;
  std::size_t value_length = value.type.Octet_String.length;

  // parse the octet string to get id and tag
  uint64_t value_id;
  uint64_t value_tag;
  std::size_t expected_length = sizeof(value_id) + sizeof(value_tag);

  if (value_length < expected_length) {
    return mk_error(env, "apdu_value_length_error");
  }
      
  std::memcpy(&value_id, received_value, sizeof(value_id));
  std::memcpy(&value_tag, received_value + sizeof(value_id), sizeof(value_tag));
       
  ERL_NIF_TERM ok =  mk_atom(env, OK);
  ERL_NIF_TERM id = enif_make_uint64(env, value_id);
  ERL_NIF_TERM tag = enif_make_uint64(env, value_tag);

  return enif_make_tuple3(env, ok, id, tag);
}

static ERL_NIF_TERM
get_uint_from_complex_ack_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary apduBin;
  
  if(
     argc != 1 ||
     !enif_inspect_binary(env, argv[0], &apduBin)
     ) {
    return enif_make_badarg(env);
  }

  uint8_t* apdu =  reinterpret_cast<uint8_t *>(apduBin.data);
  uint16_t apdu_len = apduBin.size;
  

  BACNET_APPLICATION_DATA_VALUE value;
  bool value_success = get_value_from_complex_ack(apdu, apdu_len, value);
  if (!value_success) {
    return mk_error(env, "apdu_value_error");
  }

  if (value.tag != BACNET_APPLICATION_TAG_UNSIGNED_INT) {
    return mk_error(env, "invalid_unsigned_int");
  }
  
  // get uint in the bacnet packet
  uint32_t received_value = value.type.Unsigned_Int;
       
  ERL_NIF_TERM ok =  mk_atom(env, OK);
  ERL_NIF_TERM nif_value = enif_make_uint(env, received_value);

  return enif_make_tuple2(env, ok, nif_value);
}

static ERL_NIF_TERM
get_float_from_complex_ack_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary apduBin;
  
  if(
     argc != 1 ||
     !enif_inspect_binary(env, argv[0], &apduBin)
     ) {
    return enif_make_badarg(env);
  }

  uint8_t* apdu =  reinterpret_cast<uint8_t *>(apduBin.data);
  uint16_t apdu_len = apduBin.size;
  

  BACNET_APPLICATION_DATA_VALUE value;
  bool value_success = get_value_from_complex_ack(apdu, apdu_len, value);
  if (!value_success) {
    return mk_error(env, "apdu_value_error");
  }

  if (value.tag != BACNET_APPLICATION_TAG_REAL) {
    return mk_error(env, "invalid_real");
  }
  
  // get uint in the bacnet packet
  double received_value = value.type.Real;
       
  ERL_NIF_TERM ok =  mk_atom(env, OK);
  ERL_NIF_TERM nif_value = enif_make_double(env, received_value);

  return enif_make_tuple2(env, ok, nif_value);
}

static ErlNifFunc nif_funcs[] = {
  {"build_write_msv_req_nif", 2, build_write_msv_req_nif},
  {"build_write_octetstring_req_nif", 3, build_write_octetstring_req_nif},
  
  {"build_read_octetstring_pv_req_nif", 1, build_read_octetstring_pv_req_nif},
  {"build_read_analog_value_pv_req_nif", 1, build_read_analog_value_pv_req_nif},
  {"build_read_analog_input_oos_req_nif", 1, build_read_analog_input_oos_req_nif},
    {"build_read_analog_input_pv_req_nif", 1, build_read_analog_input_pv_req_nif},
  {"build_read_analog_output_pv_req_nif", 1, build_read_analog_output_pv_req_nif},
  {"build_read_msv_pv_req_nif", 1, build_read_msv_pv_req_nif},

  {"get_apdu_from_message_nif",1,get_apdu_from_message_nif},
  {"get_pdu_type_nif", 1, get_pdu_type_nif},
  {"get_value_from_complex_ack_nif", 1, get_value_from_complex_ack_nif},
  {"get_uint_from_complex_ack_nif", 1, get_uint_from_complex_ack_nif},
  {"get_float_from_complex_ack_nif", 1, get_float_from_complex_ack_nif}
};

ERL_NIF_INIT(bacnet_utils, nif_funcs, NULL, NULL, NULL, NULL);


