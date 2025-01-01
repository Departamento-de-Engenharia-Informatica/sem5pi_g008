import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { SurgeryRoomService } from './surgery-room.service';
import json from '../../appsettings.json';

describe('SurgeryRoomService', () => {
  let service: SurgeryRoomService;
  let httpMock: HttpTestingController;

  const apiUrl = json.backendApi["1"].url + '/surgeryRoom';

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [SurgeryRoomService],
    });

    service = TestBed.inject(SurgeryRoomService);
    httpMock = TestBed.inject(HttpTestingController);
  });

  afterEach(() => {
    httpMock.verify();
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  describe('getSurgeryRooms', () => {
    it('should return an array of surgery rooms on success', () => {
      const mockSurgeryRooms = [
        { roomNumber: 1, status: 'Available' },
        { roomNumber: 2, status: 'Occupied' },
      ];

      service.getSurgeryRooms().subscribe(response => {
        expect(response).toEqual(mockSurgeryRooms);
      });

      const req = httpMock.expectOne(`${apiUrl}/status`);
      expect(req.request.method).toBe('GET');
      req.flush(mockSurgeryRooms);
    });

    it('should return an empty array on error', () => {
      const mockError = 'Error getting surgery rooms';

      service.getSurgeryRooms().subscribe(response => {
        expect(response).toEqual([]);
      });

      const req = httpMock.expectOne(`${apiUrl}/status`);
      expect(req.request.method).toBe('GET');
      req.flush(mockError, { status: 500, statusText: 'Server Error' });
    });
  });
});
