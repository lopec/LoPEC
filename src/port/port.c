/**
 * NAME        : port.c
 * DESCRIPTION : A port driver working between erlang and the C host
 * VERSION     : 0.0.1
 * OWNER       : Bjorn Dahlman
 */

#include <stdio.h>
#include "erl_driver.h"

int foo(unsigned char arg);
int bar(unsigned char arg);

typedef struct {
    ErlDrvPort port;
} olol_data;

static ErlDrvData port_drv_start(ErlDrvPort port, char *buff)
{
    olol_data* d = (olol_data*)driver_alloc(sizeof(olol_data));
    d->port = port;
    return (ErlDrvData)d;
}

static void port_drv_stop(ErlDrvData handle)
{
    driver_free((char*)handle);
}

static void port_drv_output(ErlDrvData handle, char *buff, int bufflen)
{
    olol_data* d = (olol_data*)handle;
    char fn = buff[0], arg = buff[1], res;

    switch (fn)
      {
      case 0: res = foo(arg); break;
      case 1: res = bar(arg); break;
      default: res = 0;
      }

    driver_output(d->port, &res, 1);
}

int foo(unsigned char arg)
{
  return arg + 1;
}

int bar(unsigned char arg)
{
  return arg * 2;
}

ErlDrvEntry port_entry = {
    NULL,                       /* F_PTR init, N/A */
    port_drv_start,             /* L_PTR start, called when port is opened */
    port_drv_stop,              /* F_PTR stop, called when port is closed */
    port_drv_output,            /* F_PTR output, called when erlang has sent */
    NULL,                       /* F_PTR ready_input, called when input descriptor ready */
    NULL,                       /* F_PTR ready_output, called when output descriptor ready */
    "port",                     /* char *driver_name, the argument to open_port */
    NULL,                       /* F_PTR finish, called when unloaded */
    NULL,                       /* F_PTR control, port_command callback */
    NULL,                       /* F_PTR timeout, reserved */
    NULL                        /* F_PTR outputv, reserved */
};

DRIVER_INIT(port) /* must match name in driver_entry */
{
  return &port_entry;
}
