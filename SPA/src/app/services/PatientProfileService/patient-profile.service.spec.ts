import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { PatientProfileService} from './patient-profile-service';
import { PatientProfile } from '../../Domain/PatientProfile';
import json from '../../appsettings.json';

describe('PatientProfileService', () => {
  let service: PatientProfileService;
  let httpMock: HttpTestingController;
  const apiUrl = json.backendApi["1"].url + '/Patient';

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [PatientProfileService],
    });
    service = TestBed.inject(PatientProfileService);
    httpMock = TestBed.inject(HttpTestingController);
  });

  afterEach(() => {
    httpMock.verify();
  });

  it('should register a patient profile', () => {
    const newProfile: PatientProfile = {
      id: '1',
      fullName: 'John Doe',
      email: 'john.doe@example.com',
      birthDate: '1990-01-01',
      phoneNumber: 1234567890,
      firstName: 'John',
      lastName: 'Doe',
      gender: 'Male',
      emergencyContact: 'Jane Doe, 0987654321',
    };

    service.registerPatientProfile(newProfile).subscribe((response) => {
      expect(response).toBe('Registration successful');
    });

    const req = httpMock.expectOne(`${apiUrl}/registerPatientProfile`);
    expect(req.request.method).toBe('POST');
    expect(req.request.body).toEqual(newProfile);
    req.flush('Registration successful');
  });

  it('should delete a patient profile', () => {
    const email = 'john.doe@example.com';

    service.deletePatientProfile(email).subscribe((response) => {
      expect(response).toBe('Profile deleted successfully');
    });

    const req = httpMock.expectOne(`${apiUrl}/deletePatientProfile/${email}`);
    expect(req.request.method).toBe('DELETE');
    req.flush('Profile deleted successfully');
  });

  it('should list all patient profiles', () => {
    const mockProfiles: PatientProfile[] = [
      {
        id: '1',
        fullName: 'John Doe',
        email: 'john.doe@example.com',
        birthDate: '1990-01-01',
        phoneNumber: 1234567890,
        firstName: 'John',
        lastName: 'Doe',
        gender: 'Male',
        emergencyContact: 'Jane Doe, 0987654321',
      },
      {
        id: '2',
        fullName: 'Jane Doe',
        email: 'jane.doe@example.com',
        birthDate: '1992-01-01',
        phoneNumber: 9876543210,
        firstName: 'Jane',
        lastName: 'Doe',
        gender: 'Female',
        emergencyContact: 'John Doe, 1234567890',
      },
    ];

    service.listAllPatientProfiles().subscribe((profiles) => {
      expect(profiles.length).toBe(2);
      expect(profiles).toEqual(mockProfiles);
    });

    const req = httpMock.expectOne(`${apiUrl}/`);
    expect(req.request.method).toBe('GET');
    req.flush(mockProfiles);
  });

  it('should filter patient profiles by name', () => {
    const name = 'John Doe';
    const mockProfiles: PatientProfile[] = [
      {
        id: '1',
        fullName: 'John Doe',
        email: 'john.doe@example.com',
        birthDate: '1990-01-01',
        phoneNumber: 1234567890,
        firstName: 'John',
        lastName: 'Doe',
        gender: 'Male',
        emergencyContact: 'Jane Doe, 0987654321',
      },
    ];

    service.filterPatientProfilesByName(name).subscribe((profiles) => {
      expect(profiles).toEqual(mockProfiles);
    });

    const req = httpMock.expectOne(`${apiUrl}/listPatientProfilesByName/${name}`);
    expect(req.request.method).toBe('GET');
    req.flush(mockProfiles);
  });

  it('should filter patient profiles by email', () => {
    const email = 'john.doe@example.com';
    const mockProfile: PatientProfile = {
      id: '1',
      fullName: 'John Doe',
      email: 'john.doe@example.com',
      birthDate: '1990-01-01',
      phoneNumber: 1234567890,
      firstName: 'John',
      lastName: 'Doe',
      gender: 'Male',
      emergencyContact: 'Jane Doe, 0987654321',
    };

    service.filterPatientProfilesByEmail(email).subscribe((profile) => {
      expect(profile).toEqual(mockProfile);
    });

    const req = httpMock.expectOne(`${apiUrl}/listPatientProfilesByEmail/${email}`);
    expect(req.request.method).toBe('GET');
    req.flush(mockProfile);
  });

  it('should filter patient profiles by birth date', () => {
    const birthDate = '1990-01-01';
    const mockProfiles: PatientProfile[] = [
      {
        id: '1',
        fullName: 'John Doe',
        email: 'john.doe@example.com',
        birthDate: '1990-01-01',
        phoneNumber: 1234567890,
        firstName: 'John',
        lastName: 'Doe',
        gender: 'Male',
        emergencyContact: 'Jane Doe, 0987654321',
      },
    ];

    service.filterPatientProfilesByBirthDate(birthDate).subscribe((profiles) => {
      expect(profiles).toEqual(mockProfiles);
    });

    const req = httpMock.expectOne(`${apiUrl}/listPatientProfilesByDateOfBirth/${birthDate}`);
    expect(req.request.method).toBe('GET');
    req.flush(mockProfiles);
  });

  it('should filter patient profiles by medical record number', () => {
    const id = 'MRN12345';
    const mockProfile: PatientProfile = {
      id: '1',
      fullName: 'John Doe',
      email: 'john.doe@example.com',
      birthDate: '1990-01-01',
      phoneNumber: 1234567890,
      firstName: 'John',
      lastName: 'Doe',
      gender: 'Male',
      emergencyContact: 'Jane Doe, 0987654321',
    };

    service.filterPatientProfilesByMedicalRecordNumber(id).subscribe((profile) => {
      expect(profile).toEqual(mockProfile);
    });

    const req = httpMock.expectOne(`${apiUrl}/listPatientProfilesByMedicalRecordNumber/${id}`);
    expect(req.request.method).toBe('GET');
    req.flush(mockProfile);
  });

  it('should edit a patient profile', () => {
    const email = 'john.doe@example.com';
    const editedProfile: PatientProfile = {
      id: '1',
      fullName: 'John Doe',
      email: 'john.doe@example.com',
      birthDate: '1990-01-01',
      phoneNumber: 1234567890,
      firstName: 'John',
      lastName: 'Doe',
      gender: 'Male',
      emergencyContact: 'Jane Doe, 0987654321',
    };

    service.editPatientProfile(editedProfile, email).subscribe((response) => {
      expect(response).toBe('Profile updated successfully');
    });

    const req = httpMock.expectOne(`${apiUrl}/editPatientProfile/${email}`);
    expect(req.request.method).toBe('PUT');
    expect(req.request.body).toEqual(editedProfile);
    req.flush('Profile updated successfully');
  });
});
