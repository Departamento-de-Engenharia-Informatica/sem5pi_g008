import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import {MedicalRecordAllergyService} from './medical-record-allergy.service';
import json from '../../appsettings.json';
import {DisplayMedicalRecordAllergyDTO} from '../../DTOs/displayDTOs/displayMedicalRecordAllergyDTO';

describe('MedicalRecordAllergyService', () => {
  let service: MedicalRecordAllergyService;
  let httpMock: HttpTestingController;

  const mockApiUrl = json.backendApi['2'].url + '/medicalRecord';

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [MedicalRecordAllergyService],
    });
    service = TestBed.inject(MedicalRecordAllergyService);
    httpMock = TestBed.inject(HttpTestingController);
  });


  afterEach(() => {
    httpMock.verify();
  });

  it('should list medical record allergies by medical record ID', () => {
    const mockAllergies: DisplayMedicalRecordAllergyDTO[] = [
      {
        allergy: 'ABC123',
        medicalRecordId: '123',
        doctor: 'Doctor Name',
        comment: 'Comment',
      },
      {
        allergy: 'DEF456',
        medicalRecordId: '123',
        doctor: 'Doctor Name',
        comment: 'Comment',
      },
    ];

    const medicalRecordId = '123';
    const expectedResponse = { body: mockAllergies };

    service.listAllergiesInMedicalRecord(medicalRecordId).subscribe((data) => {
      expect(data.length).toBe(2);
      expect(data[0].allergy).toBe('ABC123');
    });

    const req = httpMock.expectOne(`${mockApiUrl}/${medicalRecordId}/allergy`);
    expect(req.request.method).toBe('GET');
    req.flush(expectedResponse);
  });
});
