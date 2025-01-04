import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { MedicalRecordMedicalConditionService } from './medical-record-medical-condition.service';
import json from '../../appsettings.json';
import { DisplayMedicalRecordConditionDTO } from '../../DTOs/displayDTOs/displayMedicalRecordConditionDTO';

describe('MedicalRecordMedicalConditionService', () => {
  let service: MedicalRecordMedicalConditionService;
  let httpMock: HttpTestingController;

  const mockApiUrl = json.backendApi['2'].url + '/medicalRecord';

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [MedicalRecordMedicalConditionService],
    });
    service = TestBed.inject(MedicalRecordMedicalConditionService);
    httpMock = TestBed.inject(HttpTestingController);
  });

  afterEach(() => {
    httpMock.verify();
  });

  it('should list medical record conditions by medical record ID', () => {
    const mockConditions: DisplayMedicalRecordConditionDTO[] = [
      {
        conditionCode: 'ABC123',
        conditionDesignation: 'Condition 1',
        doctorName: 'Doctor Name',
        doctorLicenseNumber: 123,
        comment: 'Comment',
      },
      {
        conditionCode: 'DEF456',
        conditionDesignation: 'Condition 2',
        doctorName: 'Doctor Name',
        doctorLicenseNumber: 456,
        comment: 'Comment',
      },
    ];

    const medicalRecordId = '123';
    const expectedResponse = { medicalRecordConditions: mockConditions };

    service.listMedicalRecordConditionsByMedicalRecordId(medicalRecordId).subscribe((data) => {
      expect(data.medicalRecordConditions.length).toBe(2);
      expect(data.medicalRecordConditions[0].conditionDesignation).toBe('Condition 1');
    });

    const req = httpMock.expectOne(`${mockApiUrl}/${medicalRecordId}/condition`);
    expect(req.request.method).toBe('GET');
    req.flush(expectedResponse);
  });

  it('should filter medical record conditions by code', () => {
    const mockCondition: DisplayMedicalRecordConditionDTO = {
      conditionCode: 'ABC123',
      conditionDesignation: 'Condition 1',
      doctorName: 'Doctor Name',
      doctorLicenseNumber: 123,
      comment: 'Comment',
    };

    const medicalRecordId = '123';
    const code = 'ABC123';

    service.filterMedicalRecordConditionsByCode(medicalRecordId, code).subscribe((data) => {
      expect(data.filteredMedicalRecordConditions.conditionDesignation).toBe('Condition 1');
    });

    const req = httpMock.expectOne(`${mockApiUrl}/${medicalRecordId}/condition/by-code/${code}`);
    expect(req.request.method).toBe('GET');
    req.flush({ medicalRecordCondition: mockCondition });
  });

  it('should filter medical record conditions by designation', () => {
    const mockCondition: DisplayMedicalRecordConditionDTO = {
      conditionCode: 'ABC123',
      conditionDesignation: 'Condition 2',
      doctorName: 'Doctor Name',
      doctorLicenseNumber: 123,
      comment: 'Comment',
    };

    const medicalRecordId = '123';
    const designation = 'Condition 2';

    service.filterMedicalRecordConditionsByDesignation(medicalRecordId, designation).subscribe((data) => {
      expect(data.filteredMedicalRecordConditions.conditionDesignation).toBe('Condition 2');
    });

    const req = httpMock.expectOne(`${mockApiUrl}/${medicalRecordId}/condition/by-designation/${designation}`);
    expect(req.request.method).toBe('GET');
    req.flush({ medicalRecordCondition: mockCondition });
  });
});
