import { Injectable } from '@angular/core';
import json from '../../appsettings.json';
import {HttpClient, HttpErrorResponse} from '@angular/common/http';
import {catchError, map, Observable, throwError} from 'rxjs';
import {DisplayMedicalRecordConditionDTO} from '../../DTOs/displayDTOs/displayMedicalRecordConditionDTO';
import {MedicalRecordConditionMapper} from '../../DTOs/mappers/medicalRecordConditionMapper';
import {BackendMedicalRecordConditionDTO} from '../../DTOs/backendDTOs/backendMedicalRecordConditionDTO';

@Injectable({
  providedIn: 'root'
})
export class MedicalRecordMedicalConditionService {

  private apiUrl = json.backendApi["2"].url + '/medicalRecord';

  constructor(private http: HttpClient) {
  }
  getAllMedicalRecordConditions(medicalRecordId: string) : Observable<{ medicalRecordConditions: DisplayMedicalRecordConditionDTO[] }>{
    const url = `${this.apiUrl}/Allcondition`;

    return this.http
      .get<{ medicalRecordConditions: BackendMedicalRecordConditionDTO[] }>(url, {
      withCredentials: true
    }).pipe(
      map((response) => {

        const medicalRecordConditions: DisplayMedicalRecordConditionDTO[] = [];

        for (const backendMedicalRecordConditionDTO of response.medicalRecordConditions) {
          const medicalRecordConditionDomain = MedicalRecordConditionMapper.backendDisplayDTOToDomain(backendMedicalRecordConditionDTO);
          const displayMedicalRecordConditionDTO = MedicalRecordConditionMapper.domainToDisplayDTO(medicalRecordConditionDomain);

          medicalRecordConditions.push(displayMedicalRecordConditionDTO);
        }

        return { medicalRecordConditions: medicalRecordConditions };
      }),
      catchError((error: HttpErrorResponse) => {
        let errorMessage = 'An unknown error occurred.';

        if(error.status === 850) {
          errorMessage = error.error.message;
        }

        if(error.status === 900) {
          errorMessage = "This section is empty due to not founding the medical record.";
        }

        return throwError(errorMessage);
      })
    );
  }

  listMedicalRecordConditionsByMedicalRecordId(medicalRecordId: string) : Observable<{ medicalRecordConditions: DisplayMedicalRecordConditionDTO[] }>{
    const url = `${this.apiUrl}/${medicalRecordId}/condition`;

    return this.http
      .get<{ medicalRecordConditions: BackendMedicalRecordConditionDTO[] }>(url, {
      withCredentials: true
    }).pipe(
      map((response) => {

        const medicalRecordConditions: DisplayMedicalRecordConditionDTO[] = [];

        for (const backendMedicalRecordConditionDTO of response.medicalRecordConditions) {
          const medicalRecordConditionDomain = MedicalRecordConditionMapper.backendDisplayDTOToDomain(backendMedicalRecordConditionDTO);
          const displayMedicalRecordConditionDTO = MedicalRecordConditionMapper.domainToDisplayDTO(medicalRecordConditionDomain);

          medicalRecordConditions.push(displayMedicalRecordConditionDTO);
        }

        return { medicalRecordConditions: medicalRecordConditions };
      }),
      catchError((error: HttpErrorResponse) => {
        let errorMessage = 'An unknown error occurred.';

        if(error.status === 850) {
          errorMessage = error.error.message;
        }

        if(error.status === 900) {
          errorMessage = "This section is empty due to not founding the medical record.";
        }

        return throwError(errorMessage);
      })
    );
  }

  filterMedicalRecordConditionsByCode(medicalRecordId: string, code: string) : Observable<{ filteredMedicalRecordConditions: DisplayMedicalRecordConditionDTO }> {
    const url = `${this.apiUrl}/${medicalRecordId}/condition/by-code/${code}`;

    console.log(url);

    return this.http
      .get<{ medicalRecordCondition: BackendMedicalRecordConditionDTO }>(url, {
        withCredentials: true
      }).pipe(
        map((response) => {

            const medicalRecordConditionDomain = MedicalRecordConditionMapper.backendDisplayDTOToDomain(response.medicalRecordCondition);
            const displayMedicalRecordConditionDTO = MedicalRecordConditionMapper.domainToDisplayDTO(medicalRecordConditionDomain);

          return { filteredMedicalRecordConditions: displayMedicalRecordConditionDTO };
        }),
        catchError((error: HttpErrorResponse) => {
          let errorMessage = 'An unknown error occurred.';

          if(error.status === 900) {
            errorMessage = error.error.message;
          }

          if(error.status >= 801 && error.status <= 809) {
            errorMessage = error.error.message;
          }

          if (error.status === 810) {
            errorMessage = error.error.message;
          }

          if (error.status === 851) {
            errorMessage = error.error.message;
          }

          return throwError(errorMessage);
        })
      );

  }

  filterMedicalRecordConditionsByDesignation(medicalRecordId: string, designation: string) : Observable<{ filteredMedicalRecordConditions: DisplayMedicalRecordConditionDTO }> {
    const url = `${this.apiUrl}/${medicalRecordId}/condition/by-designation/${designation}`;

    console.log(url);

    return this.http
      .get<{ medicalRecordCondition: BackendMedicalRecordConditionDTO }>(url, {
        withCredentials: true
      }).pipe(
        map((response) => {

          const medicalRecordConditionDomain = MedicalRecordConditionMapper.backendDisplayDTOToDomain(response.medicalRecordCondition);
          const displayMedicalRecordConditionDTO = MedicalRecordConditionMapper.domainToDisplayDTO(medicalRecordConditionDomain);

          return { filteredMedicalRecordConditions: displayMedicalRecordConditionDTO };
        }),
        catchError((error: HttpErrorResponse) => {
          let errorMessage = 'An unknown error occurred.';

          if(error.status === 900) {
            errorMessage = error.error.message;
          }

          if(error.status >= 801 && error.status <= 809) {
            errorMessage = error.error.message;
          }

          if (error.status === 810) {
            errorMessage = error.error.message;
          }

          if (error.status === 851) {
            errorMessage = error.error.message;
          }

          return throwError(errorMessage);
        })
      );

  }

  listMedicalRecordConditions(): Observable<{ medicalRecordConditions: DisplayMedicalRecordConditionDTO[] }> {
    const url = `${this.apiUrl}/Allcondition`;
    console.log('Loading medical record conditions...');

    return this.http.get<{ medicalRecordConditions: BackendMedicalRecordConditionDTO[] }>(url, { withCredentials: true })
      .pipe(
        map((response) => {
console.log('MRespopnse:', response);
          console.log('Medical record conditions loaded:', response.medicalRecordConditions);
          const medicalRecordConditions = response.medicalRecordConditions.map(
            (backendDTO) => MedicalRecordConditionMapper.domainToDisplayDTO(
              MedicalRecordConditionMapper.backendDisplayDTOToDomain(backendDTO)
            )
          );
          return { medicalRecordConditions };
        }),
        catchError((error: HttpErrorResponse) => {
          let errorMessage = 'An unknown error occurred.';
          if (error.status === 900) {
            errorMessage = error.error.message;
          }
          return throwError(errorMessage);
        })
      );
  }

  saveSelectedMedicalRecordConditions(conditions: any[]): Observable<any> {
    const url = `${this.apiUrl}/saveConditions`;
    return this.http.post(url, { conditions }, { withCredentials: true }).pipe(
      map((response) => {
        console.log('Conditions saved successfully:', response);
        return response;
      }),
      catchError((error: HttpErrorResponse) => {
        console.error('Failed to save conditions:', error);
        return throwError('Failed to save conditions.');
      })
    );
  }


}
