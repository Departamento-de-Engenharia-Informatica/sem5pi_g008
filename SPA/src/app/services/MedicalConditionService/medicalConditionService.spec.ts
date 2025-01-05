import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { MedicalConditionService } from './medicalConditionService';
import { MedicalConditionMapper } from '../../DTOs/mappers/medicalConditionMapper';
import { MedicalConditionDTO } from '../../DTOs/GenericDTOs/medicalConditionDTO';
import json from '../../appsettings.json';

describe('MedicalConditionService', () => {
  let service: MedicalConditionService;
  let httpMock: HttpTestingController;
  let mockMapper: jasmine.SpyObj<typeof MedicalConditionMapper>;

  const apiUrl = `${json.backendApi['2'].url}/medicalConditions`;

  beforeEach(() => {
    mockMapper = jasmine.createSpyObj('MedicalConditionMapper', ['dtoToDomain', 'domainToBackendDto']);

    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [
        MedicalConditionService,
        { provide: MedicalConditionMapper, useValue: mockMapper },
      ],
    });

    service = TestBed.inject(MedicalConditionService);
    httpMock = TestBed.inject(HttpTestingController);
  });

  afterEach(() => {
    httpMock.verify();
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should add a new medical condition', () => {
    const mockMedicalConditionDTO: MedicalConditionDTO = {
      code: 'MC123',
      designation: 'Hypertension',
      description: 'High blood pressure',
      symptomsList: ['Headache', 'Blurred Vision', 'Fatigue'],
    };

    const mockMedicalCondition = {
      code: 'MC123',
      designation: 'Hypertension',
      description: 'High blood pressure',
      symptomsList: ['Headache', 'Blurred Vision', 'Fatigue'],
    };

    const mockResponse = 'Medical condition added successfully';

    mockMapper.dtoToDomain.and.returnValue({ ...mockMedicalCondition });
    mockMapper.domainToBackendDto.and.returnValue({ ...mockMedicalCondition });

    service.addMedicalCondition(mockMedicalConditionDTO).subscribe((response) => {
      expect(response).toBe(mockResponse);
    });

    const req = httpMock.expectOne(apiUrl);
    expect(req.request.method).toBe('POST');
    expect(req.request.body.code).toEqual(mockMedicalCondition.code);
    expect(req.request.body.designation).toEqual(mockMedicalCondition.designation);
    expect(req.request.body.description).toEqual(mockMedicalCondition.description);
    expect(req.request.body.symptomsList).toEqual(mockMedicalCondition.symptomsList);
    expect(req.request.withCredentials).toBeTrue();
    req.flush(mockResponse);
  });

  it('should update medical condition description', () => {
    const medicalConditionId = '123';
    const newDescription = 'New Description';

    service.updateMedicalConditionDescription(medicalConditionId, newDescription).subscribe((response: any) => {
      expect(response).toBeUndefined();
    });

    const req = httpMock.expectOne(`${apiUrl}/${medicalConditionId}/description`);
    expect(req.request.method).toBe('PATCH');
    expect(req.request.body).toEqual({ description: newDescription });
  });

  it('should update medical condition symptoms', () => {
    const medicalConditionId = '123';
    const newSymptoms = ['Symptom1', 'Symptom2'];

    service.updateMedicalConditionSymptoms(medicalConditionId, newSymptoms).subscribe((response: any) => {
      expect(response).toBeUndefined();
    });

    const req = httpMock.expectOne(`${apiUrl}/${medicalConditionId}/symptoms`);
    expect(req.request.method).toBe('PATCH');
    expect(req.request.body).toEqual({ symptomsList: newSymptoms });
  });
});
