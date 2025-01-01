import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { MedicalConditionService } from './medicalConditionService';
import { MedicalConditionMapper } from '../../DTOs/mappers/medicalConditionMapper';
import { MedicalConditionDTO } from '../../DTOs/GenericDTOs/medicalConditionDTO';
import { BackendMedicalConditionDTO } from '../../DTOs/backendDTOs/backendMedicalConditionDTO';
import json from '../../appsettings.json';

describe('MedicalConditionService', () => {
  let service: MedicalConditionService;
  let httpMock: HttpTestingController;
  let mockMapper: jasmine.SpyObj<typeof MedicalConditionMapper>;

  const apiUrl = `${json.backendApi['2'].url}/medicalConditions`;

  beforeEach(() => {
    // Create a spy object for the MedicalConditionMapper
    mockMapper = jasmine.createSpyObj('MedicalConditionMapper', ['dtoToDomain', 'domainToBackendDto']);

    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule], // Import HttpClientTestingModule
      providers: [
        MedicalConditionService, // Provide the service under test
        { provide: MedicalConditionMapper, useValue: mockMapper }, // Provide the mocked mapper
      ],
    });

    service = TestBed.inject(MedicalConditionService); // Inject the service
    httpMock = TestBed.inject(HttpTestingController); // Inject the HttpTestingController
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
});
