#ifndef __HELPER_H__
#define __HELPER_H__

#include <yaml.h>

typedef struct buffer_s {
	unsigned char *buff;
	unsigned int size, used;
} buffer_t;
void buffer_init(buffer_t *buffer);
int buffer_append(void *ext, unsigned char *str, size_t size);
unsigned char * get_buffer_buff(buffer_t *b);
unsigned int get_buffer_used(buffer_t *b);

void my_emitter_set_output(yaml_emitter_t *e, buffer_t *b);

unsigned char const * get_parser_error_problem(yaml_parser_t *p);
unsigned char const * get_parser_error_context(yaml_parser_t *p);
unsigned int    get_parser_error_offset(yaml_parser_t *p);

unsigned char const * get_emitter_error(yaml_emitter_t *e);

int simple_document_start(yaml_event_t *e);

int get_event_type(yaml_event_t *e);

unsigned char * get_scalar_value(yaml_event_t *e);
unsigned long get_scalar_length(yaml_event_t *e);

unsigned char * get_scalar_tag(yaml_event_t *e);
unsigned long get_scalar_tag_len(yaml_event_t *e);

int get_scalar_style(yaml_event_t *e);

unsigned char * get_scalar_anchor(yaml_event_t *e);
unsigned char * get_sequence_start_anchor(yaml_event_t *e);
unsigned char * get_mapping_start_anchor(yaml_event_t *e);
unsigned char * get_alias_anchor(yaml_event_t *e);

#endif /* __HELPER_H__ */
