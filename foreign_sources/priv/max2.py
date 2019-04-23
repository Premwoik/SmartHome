from pymax.cube import Discovery
from pymax.cube import Cube
from pymax.response import SingleLResponse
from pymax.objects import Device, DeviceList
from typing import Any, Dict
import sys
import json
from pipe import *

# SERIALIZERS

def serialize_settings(s : SingleLResponse) -> Dict:
    return {'temperature': s.temperature
            , 'actual_temperature' : s.actual_temperature}

def serialize_device(d) -> Dict:
    return {
        'name' : d.name if 'name' in d else None,
        'rf_address' : str(d.rf_address) if 'rf_address' in d else None,
        'settings': serialize_settings(d.settings) if 'settings' in d else None
    }

def serialize_cube(c : Cube) -> Dict:
    return {
        'devices' : c.devices | select(lambda x: serialize_device(x)) | as_list(),
        'rooms' : c.rooms | select(lambda x: serialize_room(x)) | as_list()
    }

def serialize_room(r) -> Dict:
    return {
        'room_id' : r.room_id,
        'name' : r.name,
        'rf_address' : str(r.rf_address),
        'devices' : r.devices | select(lambda x: serialize_device(x)) | as_list
    }

# FUNCTIONS

def execute(func):
    with Cube(sys.argv[1]) as cube:
        return func(cube)


def set_mode_auto():
    if len(sys.argv) == 5:
        room_id = int(sys.argv[3])
        addr = sys.argv[4]
        f = lambda c: c.set_mode_auto(room_id, addr).command_success
        return execute(f)

def set_mode_manual():
    if len(sys.argv) == 6:
            room_id = int(sys.argv[3])
            addr = sys.argv[4]
            temp = float(sys.argv[5])
            f = lambda c: c.set_mode_manual(room_id, addr, temp).command_success
            return execute(f)


def set_mode_boost():
    if len(sys.argv) == 5:
        room_id = int(sys.argv[3])
        addr = sys.argv[4]
        f = lambda c: c.set_mode_boost(room_id, addr).command_success
        return execute(f)

def read():
    f = lambda c: serialize_cube(c)
    return execute(f)

# MAIN

if len(sys.argv) > 2:
    switcher = {
        'read' : read,
        'set_mode_manual' : set_mode_manual,
        'set_mode_auto' : set_mode_auto,
        'set_mode_boost' : set_mode_boost
    }
    func = switcher.get(sys.argv[2], lambda: None)
    print(json.dumps(func()))
else:
    print(json.dumps(None))


